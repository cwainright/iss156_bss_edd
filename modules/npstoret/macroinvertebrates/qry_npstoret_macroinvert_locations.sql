SELECT tblLocations.LocSTATN_ORG_ID AS Org_Code
	,tblLocations.StationID AS Park_Code
	,tblLocations.StationID AS Location_ID
	,tblLocations.[Station Name]
	,tblLocations.LocationType AS Location_Type
	,tblLocations.[Latitude Degrees] AS Latitude
	,tblLocations.[Longitude Degrees] AS Longitude
	,tblLocations.[Geopositioning Method] AS Lat_Lon_Method
	,tblLocations.[Geopositioning Datum] AS Lat_Lon_Datum
	,tblLocations.Scale AS Source_Map_Scale_Numeric
	,'' AS Lat_Lon_Accuracy
	,'' AS Lat_Lon_Accuracy_Unit
	,tblLocations.Description AS Location_Description
	,tblLocations.[Travel Directions] AS Travel_Directions
	,tblLocations.LocationPurpose AS Location_Purpose
	,'' AS Establishment_Date
	,tblLocations.HUC AS HUC8_Code
	,'' AS HUC12_Code
	,'' AS Alternate_Location_ID
	,'' AS Alternate_Location_ID_Context
	,tblLocations.Elevation AS Elevation
	,tblLocations.[Elevation Units] AS Elevation_Unit
	,tblLocations.[Elevation Method] AS Elevation_Method
	,tblLocations.[Elevation Datum] AS Elevation_Datum
	,tblLocations.ElevationAccuracy AS Elevation_Accuracy
	,'ft' AS Elevation_Accuracy_Unit
	,'US' AS Elevation_Country_Code_Unit
	,tblLocations.STATE AS State_Code
	,tblLocations.County AS County_Name
	,tblLocations.DrainageArea AS Drainage_Area
	,tblLocations.DrainageAreaUnits AS Drainage_Area_Unit
	,tblLocations.ContributingDrainageArea AS Contributing_Area
	,tblLocations.DrainageAreaUnits AS Contributing_Area_Unit
	,'' AS Tribal_Land_Indicator
	,'' AS Tribal_Land_Name
	,'' AS Well_ID
	,'' AS Well_Type
	,'' AS Aquifer_Name
	,'' AS Formation_Type
	,'' AS Well_Hole_Depth
	,'' AS Well_Hole_Depth_Unit
	,'' AS Well_Status
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
