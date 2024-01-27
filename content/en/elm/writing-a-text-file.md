---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file means saving data in a file on disk in text format. Programmers do it for data storage, configuration, logging, or exporting human-readable reports.

## How to:

Elm is a front-end web language, so it can't directly write files to a disk. But it can trigger a download with the desired content. To simulate file writing, we'll create a text and use a link to download it as a file.

```Elm
module Main exposing (main)

import Browser
import Html exposing (Html, a, text, attribute)
import Html.Attributes exposing (href)

createTextFileContent : String
createTextFileContent =
    "Hello, World! This is some content."

createDownloadHref : String -> String
createDownloadHref content =
    "data:text/plain;charset=utf-8," ++ encodeURIComponent(content)

main : Html msg
main =
    a [ href (createDownloadHref createTextFileContent), attribute "download" "myTextFile.txt" ]
        [ text "Download Text File" ]
```

Sample output is a clickable link that downloads 'myTextFile.txt' containing "Hello, World! This is some content."

## Deep Dive

Elm runs in the browser, so functions needed to write directly to the file system aren't available. Historically, JavaScript has similar limitations due to browser security constraints. However, newer web APIs and Elm's interop feature (`Ports`) allow triggering downloads or handling file system access in web applications. Alternatives are using server-side programming languages for direct file manipulation or relying on web APIs like the File System Access API for extended abilities in modern browsers.

## See Also

- Elm Official Guide on JavaScript Interop (Ports): [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
- The `File` Web API for advanced file handling in browsers: [MDN Web Docs - File API](https://developer.mozilla.org/en-US/docs/Web/API/File)
- A broader look into the Elm architecture: [Official Elm Architecture](https://guide.elm-lang.org/architecture/)
