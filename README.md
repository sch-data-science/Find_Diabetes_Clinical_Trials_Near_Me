# Find Diabetes-related Clinical Trials Near Me

This repository stores the code for the <a href='https://schdatascience-find-diabetes-related-clinical-trials-near-me.share.connect.posit.cloud/' target="_blank" rel="noopener noreferrer">"Find Diabetes-related Clinical Trials Near Me" app</a> hosted on Posit Connect Cloud.

Each day, at 5:25 UTC, the github/workflows/main.yml starts an instance of R, loads the necessary R libraries, executes the file, "getdata.R", and then commits the results to this repository.  In order for this file to pull and push to your repository, you will have to add your GitHub token will need to be saved in your Repository secrets.
To add to your token to your Repository secrets, go to you Repository home page, click 'Settings' (gear icon in upper right), then scroll down to the Security section on the left-side bar, click 'Secrets and variables', then 'Actions'.  Note, you only need to do this if you are wanting to automate the daily data refresh.  

The "getdata.R" file pulls data from <a target="_blank"  rel="noopener noreferrer" href='https://clinicaltrials.gov/'>ClinicalTrails.gov</a> in the form of a JSON file and then parses that JSON file into a dataframe.
Next, the file uses the <a target="_blank" rel="noopener noreferrer" href='https://www.mapbox.com/'>Mapbox API </a> to pull in geocoordinates for those study sites whose geocoordinates are missing in the data provided by ClinicalTrials.gov.
For this to work, you will need to register for a <a target="_blank" rel="noopener noreferrer" href='https://www.mapbox.com/'>Mapbox API key</a> which is available for free when you sign up for the free tier of a Mapbox account.  You will then need to save this in your Repository secrets as described above for the 'main.yml' file.
The data is then formatted to suit the needs of Shiny app (the app.R file) and saved as "trials.RDS" so that it can be used by the Shiny app.

A number of customizations have been made to "getdata.R" file compared to the more generic <a href="https://schdatascience-find-clinical-trials-near-me.share.connect.posit.cloud/">"Find Clinical Trials Near Me" app</a>. Users can alter/delete/add-to these customizations to suit their particular needs.  Note, changes will need to be made to the "app.R" file to accomodate changes to these customizations and are described below.
<ul>
<li> Lines 121-123: Filter the clinical trials to only include those that have the word "diabetes" in either the Condition, Brief Summary, or Keyword section of the data from ClinicalTrials.gov</li>
<li> Line 166:  Adds the name, e-mail, and phone number for the contact for the clinical trail at that location.</li>
<li> Lines 167-261: Create custom groupings to be used as drop-down options for narrowing the search.</li>
</ul>
The "app.R" file has the code for the Shiny app interface. A number of customizations have been made to this file compared to the more generic <a href="https://schdatascience-find-clinical-trials-near-me.share.connect.posit.cloud/">"Find Clinical Trials Near Me" app</a>. Users can alter/delete/add-to these customizations to suit their particular needs.
<br>
<br>

<ul>
<li> Lines 21-22: List the names of the customized groupings defined in the "getdata.R"" file on line 21 and then how you'd like them displayed in the drop-down on line 22.</li>
<li> Lines 113-133: Provide the code for the customized groupings drop-down </li>
<li> Lines 240-241: Filter the data based on the chosen customized groupings</li>
<li> Lines 297-303: Add the Contact information that was created in the "getdata.R" file and provides the column heading</li>
</ul>


The "manifest.json" file is used by <a target="_blank"  rel="noopener noreferrer" href="https://connect.posit.cloud/">Posit Connect Cloud</a> and developers should consult the <a href="https://docs.posit.co/connect-cloud/how-to/r/dependencies.html">documentation there</a>. 

Both the "getdata.R" and "app.R" files can be downloaded and run locally, though you will need to have your Mapbox API Key stored in your local .REnvironment.
To make a modified app available to others, please fork this repository to your own public GitHub repository and sign up for <a target="_blank" rel="noopener noreferrer" href="https://connect.posit.cloud/">Posit Connect Cloud</a> account, both of which have free tiers available. Forking this repository will allow us to track how this App is being built upon and used by others.
By linking your GitHub Repository to your Posit Connect Cloud account, your app will be updated every time a new "trials.RDS" file is created.
