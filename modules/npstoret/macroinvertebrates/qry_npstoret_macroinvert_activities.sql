SELECT tblActivities.LocFdAct_ORG_ID AS Org_Code
	,'Perennial stream water monitoring' AS Project_ID
	,tblLocations.StationID AS Location_ID
	,tblActivities.ID_CODE AS Activity_ID
	,'Field Msr/Obs' AS Activity_Type
	,tblCharacteristics.MEDIUM AS Medium
	,'' AS Medium_Subdivision
	,'' AS Assemblage_Sampled_Name
	,tblVisits.START_DATE AS Activity_Start_Date
	,tblVisits.START_TIME AS Activity_Start_Time
	,'Eastern Time - Washington, DC' AS Activity_Start_Time_Zone
	,'' AS Activity_End_Date
	,'' AS Activity_End_Time
	,'' AS Activity_End_Time_Zone
	,'' AS Activity_Relative_Depth
	,'' AS Activity_Depth
	,'' AS Activity_Depth_Unit
	,'' AS Activity_Upper_Depth
	,'' AS Activity_Lower_Depth
	,'' AS Activity_Depth_Reference
	,tblLocations.[Station Name] AS Additional_Location_Info
	,'' AS Activity_Sampler
	,tblVisits.D_USERID_CREATOR AS Activity_Recorder
	,tblActivities.CHAIN_OF_CUSTODY_ID AS Custody_ID
	,tblActivities.LocFdAct_ORG_ID AS Activity_Conducting_Organization
	,tblVisits.VISIT_COMMENT AS Station_Visit_Comment
	,tblActivities.ACTIVITY_COMMENT AS Activity_Comment
	,'version 2; protocol date 2009-06-30' AS Collection_Method_ID
	,'' AS Collection_Equipment_Name
	,'' AS Collection_Equipment_Description
	,'' AS Gear_Deployment
	,'' AS Container_Type
	,'' AS Container_Color
	,'' AS Container_Size
	,'' AS Container_Size_Unit
	,'version 2; protocol date 2009-06-30' AS Preparation_Method_ID
	,'' AS Chemical_Preservative
	,'' AS Thermal_Preservative
	,'' AS Transport_Storage_Description
	,'' AS Activity_Group_ID
	,tblLocations.StationID AS Activity_Group_Name
	,'' AS Activity_Group_Type
	,'' AS Collection_Duration
	,'' AS Collection_Duration_Unit
	,tblActivities.SamplingComponentName AS Sampling_Component_Name
	,tblActivities.SamplingComponentOrder AS Sampling_Component_Place_In_Series
	,'' AS Reach_Length
	,'' AS Reach_Length_Unit
	,'' AS Reach_Width
	,'' AS Reach_Width_Unit
	,'' AS Pass_Count
	,'' AS Net_Type
	,'' AS Net_Surface_Area
	,'' AS Net_Surface_Area_Unit
	,'' AS Net_Mesh_Size
	,'' AS Net_Mesh_Size_Unit
	,'' AS Boat_Speed
	,'' AS Boat_Speed_Unit
	,'' AS Current_Speed
	,'' AS Current_Speed_Unit
	,'' AS Toxicity_Test_Type
	,'' AS Effort
	,'' AS Effort_Unit
FROM (
	tblLocations INNER JOIN tblVisits ON (tblLocations.LocSTATN_ORG_ID = tblVisits.LocSTATN_ORG_ID)
		AND (tblLocations.LocSTATN_IS_NUMBER = tblVisits.LocSTATN_IS_NUMBER)
	)
INNER JOIN (
	tblCharacteristics INNER JOIN (
		tblActivities INNER JOIN tblResults ON (tblActivities.LocFdAct_ORG_ID = tblResults.LocFdAct_Org_ID)
			AND (tblActivities.LocFdAct_IS_NUMBER = tblResults.LocFdAct_IS_NUMBER)
		) ON (tblCharacteristics.LocCHDEF_ORG_ID = tblResults.LocChDef_Org_ID)
		AND (tblCharacteristics.LocCHDEF_IS_NUMBER = tblResults.LocChDef_IS_NUMBER)
	) ON (tblVisits.LocStVst_ORG_ID = tblActivities.LocStVst_ORG_ID)
	AND (tblVisits.LocStVst_IS_NUMBER = tblActivities.LocStVst_IS_NUMBER)
WHERE (
		((tblVisits.START_DATE) > #1 / 1 / 2005 #)
		AND (
			(tblResults.LocChDef_IS_NUMBER) > 30
			AND (tblResults.LocChDef_IS_NUMBER) < 55
			)
		);
