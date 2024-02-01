---
title:                "Checking if a directory exists"
date:                  2024-02-01T13:42:09.483765-07:00
model:                 gpt-4-0125-preview
simple_title:         "Checking if a directory exists"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in Google Apps Script helps you avoid errors that occur when trying to access or perform operations on a directory that doesn't exist. Programmers do this to ensure their scripts run smoothly by validating paths before proceeding with file operations.

## How to:

In Google Apps Script, we don't have a direct method like `exists()` to check for a directory's existence. Instead, we use a workaround involving `getFoldersByName()` and checking if the returned iterator has any entries. Here’s how you do it:

1. **Get the target folder's parent directory.**
2. **Search for the folder by name.**
3. **Check if any folders were found.**

```google-apps-script
function checkIfDirectoryExists(dirName) {
  // Assuming you're looking in the root directory
  var parentFolder = DriveApp.getRootFolder();
  
  // Get all folders with the specified name (case-sensitive)
  var folders = parentFolder.getFoldersByName(dirName);
  
  // Check if the folder exists by seeing if there's at least one result
  if (folders.hasNext()) {
    Logger.log(dirName + " exists.");
    return true;
  } else {
    Logger.log(dirName + " does not exist.");
    return false;
  }
}

// Example Usage
var dirExists = checkIfDirectoryExists("MySampleDirectory");
Logger.log(dirExists); // Output: true or false
```

This method leverages the `DriveApp` class to navigate the Google Drive's directory tree, searching for the target directory by name.

## Deep Dive

The approach used here, while straightforward in Google Apps Script, contrasts with many traditional programming environments where a direct query like `exists()` is available for both files and directories. This indirect method of searching by name and checking for presence in an iterator is a bit more cumbersome but necessary due to the way Google Apps Script exposes Drive's file and folder structures to scripts.

There's a notable limitation to consider: if there are multiple directories with the same name (which Google Drive allows), this method will only check for the existence of at least one directory with the specified name. It won't differentiate between multiple instances. In scenarios where exact path validation is crucial, additional logic would be required to traverse the folder hierarchy and verify the precise path.

While this method is effective within the context of Google Apps Script, developers coming from other environments might find it less intuitive. Alternatives or enhancements to this method, such as incorporating more detailed path checks, depend on the specific requirements of your application and the complexity you're willing to manage in your script.
