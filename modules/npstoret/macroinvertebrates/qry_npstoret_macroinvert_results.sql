SELECT tblResults.LocRSULT_ORG_ID AS Org_Code
	,tblActivities.ID_CODE AS Activity_ID
	,tblCharacteristics.LocCharNameCode AS Characteristic_Name
	,'' AS Method_Speciation
	,tblCharacteristics.SMPL_FRAC_TYPE_NM AS Filtered_Fraction
	,'' AS Result_Detection_Condition
	,tblResults.RESULT_TEXT AS Result_Text
	,'count' AS Result_Unit
	,tblResults.MEASURE_QUALIFIER AS Result_Qualifier
	,tblResults.VALUE_STATUS AS Result_Status
	,tblResults.VALUE_TYPE AS Result_Type
	,tblResults.RESULT_COMMENT AS Result_Comment
	,tblResults.DETECTION_LIMIT AS Method_Detection_Limit
	,tblResults.LOWER_QUANT_LIMIT AS Lower_Quantification_Limit
	,tblResults.UPPER_QUANT_LIMIT AS Upper_Quantification_Limit
	,tblResults.DET_QUANT_DESCRIPTION AS Limit_Comment
	,tblCharacteristics.TEMP_BASIS_LVL_NM AS Temperature_Basis
	,'' AS Statistical_Basis
	,tblCharacteristics.DUR_BASIS_TYPE_NM AS Time_Basis
	,tblCharacteristics.WT_BASIS_TYPE_NM AS Weight_Basis
	,'' AS Particle_Size_Basis
	,tblResults.PRECISION AS [Precision]
	,'' AS Bias
	,tblResults.CONFIDENCE_LEVEL AS Confidence_Interval
	,tblResults.Upper_Confidence_Limit AS Upper_Confidence_Limit
	,tblResults.Lower_Confidence_Limit AS Lower_Confidence_Limit
	,'' AS Result_Sampling_Point_Name
	,'' AS Result_Depth_Height_Measure
	,'' AS Result_Depth_Height_Measure_Unit
	,'' AS Result_Depth_Altitude_Reference_Point
	,'' AS Analytical_Method_ID
	,tblResults.LAB_REMARKS AS Analytical_Remark
	,tblCharacteristics.FIELD_LAB AS Lab_ID
	,'' AS Lab_Remark_Code
	,tblResults.Analysis_Date AS Analysis_Start_Date
	,tblResults.Analysis_Time AS Analysis_Start_Time
	,tblResults.Analysis_Time_Zone AS Analysis_Start_Time_Zone
	,'' AS Lab_Accreditation_Indicator
	,'' AS Lab_Accreditation_Authority_Name
	,'' AS Lab_Batch_ID
	,'' AS Lab_Sample_Preparation_ID
	,'' AS Lab_Sample_Preparation_Start_Date
	,'' AS Lab_Sample_Preparation_Start_Time
	,'' AS Lab_Sample_Preparation_Start_Time_Zone
	,'' AS Dilution_Factor
	,'' AS Num_of_Replicates
	,'' AS Data_Logger_Line_Name
	,'' AS Biological_Intent
	,'' AS Biological_Individual_ID
	,'' AS Subject_Taxon
	,'' AS Unidentified_Species_ID
	,'' AS Tissue_Anatomy
	,'' AS Group_Summary_Count_or_Weight
	,'' AS Group_Summary_Count_or_Weight_Unit
	,'' AS Cell_Form
	,'' AS Cell_Shape
	,'' AS Habit_Name_1
	,'' AS Habit_Name_2
	,'' AS Habit_Name_3
	,'' AS Voltinism
	,'' AS Pollution_Tolerance
	,'' AS Pollution_Tolerance_Scale
	,'' AS Trophic_Level
	,'' AS Functional_Feeding_Group_1
	,'' AS Functional_Feeding_Group_2
	,'' AS Functional_Feeding_Group_3
	,'' AS Resource_ID
	,'' AS Resource_Date
	,'' AS Resource_Title_Name
	,'' AS Resource_Creator_Name
	,'' AS Resource_Publisher_Name
	,'' AS Resource_Publication_Year
	,'' AS Resource_Volume_Pages
	,'' AS Resource_Subject_Text
	,'' AS Frequency_Class_Descriptor_1
	,'' AS Frequency_Class_Bounds_Unit_1
	,'' AS Frequency_Class_Lower_Bound_1
	,'' AS Frequency_Class_Upper_Bound_1
	,'' AS Frequency_Class_Descriptor_2
	,'' AS Frequency_Class_Bounds_Unit_2
	,'' AS Frequency_Class_Lower_Bound_2
	,'' AS Frequency_Class_Upper_Bound_2
	,'' AS Frequency_Class_Descriptor_3
	,'' AS Frequency_Class_Bounds_Unit_3
	,'' AS Frequency_Class_Lower_Bound_3
	,'' AS Frequency_Class_Upper_Bound_3
	,'' AS Taxonomist_Accreditation_Indicator
	,'' AS Taxonomist_Accreditation_Authority_Name
	,'data/npstoret/NCRNNPSTORET_BE_20230212.mdb' AS Result_File_Name
FROM tblProjects
INNER JOIN (
	(
		tblLocations INNER JOIN tblVisits ON (tblLocations.LocSTATN_ORG_ID = tblVisits.LocSTATN_ORG_ID)
			AND (tblLocations.LocSTATN_IS_NUMBER = tblVisits.LocSTATN_IS_NUMBER)
		) INNER JOIN (
		tblCharacteristics INNER JOIN (
			tblActivities INNER JOIN tblResults ON (tblActivities.LocFdAct_ORG_ID = tblResults.LocFdAct_Org_ID)
				AND (tblActivities.LocFdAct_IS_NUMBER = tblResults.LocFdAct_IS_NUMBER)
			) ON (tblCharacteristics.LocCHDEF_ORG_ID = tblResults.LocChDef_Org_ID)
			AND (tblCharacteristics.LocCHDEF_IS_NUMBER = tblResults.LocChDef_IS_NUMBER)
		) ON (tblVisits.LocStVst_ORG_ID = tblActivities.LocStVst_ORG_ID)
		AND (tblVisits.LocStVst_IS_NUMBER = tblActivities.LocStVst_IS_NUMBER)
	) ON (tblProjects.LocProj_ORG_ID = tblVisits.LocProj_ORG_ID)
	AND (tblProjects.LocProj_IS_NUMBER = tblVisits.LocProj_IS_NUMBER)
WHERE (
		((tblVisits.START_DATE) > #1 / 1 / 2005 #)
		AND (
			(tblResults.LocChDef_IS_NUMBER) > 30
			AND (tblResults.LocChDef_IS_NUMBER) < 55
			)
		AND ((tblProjects.ProjectID) = 'NCRNWQ01')
		);
