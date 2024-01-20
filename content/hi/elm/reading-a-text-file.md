---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

1. एक पाठ संचिका (text file) पढ़ना मतलब उसकी सामग्री पढ़ना - अनुचित, मनुष्य-अनुवादित माहिती। 
2. प्रोग्रामर्स कार्यक्रम (program) लेखन, विश्लेषण और डेटा निकलने के लिए इसे करते हैं।

## कैसे करें:

एल्म में Web API द्वारा फ़ाइल पढ़ना नहीं है। लेकिन, आप बाहरी JavaScript का उपयोग करके इसे कर सकते हैं।

```Elm
port module Main exposing (..)

port readFile : String -> Cmd msg

main =
    readFile "filename.txt"
```

इसे JavaScript में डाक कस्टम कॉल के साथ उपयोग करें।

```JavaScript
var app = Elm.Main.init();
app.ports.readFile.subscribe(function(filename) {
    var reader = new FileReader();
    reader.onload = function(event) {
        console.log(event.target.result);
    };
    reader.readAsText(new File([""], filename));
});
```

## गहराई में:

1. ऐतिहासिक प्रसंग: पाठ फाइल की पढ़ाई से अनुवादन और विश्लेषण में सहयोग मिलता है। यह संकेतक के लिए काम करता है।

2. विकल्प: अन्य भाषाओं में भी कैसे पठन का समर्थन होता है। जैसे Python में ऐसा किया जाता है।

3. कार्यान्वयन विवरण: एल्म किसी गोपनीयता विज्ञप्त और ट्रांजेक्शन सुरक्षा कारणों के लिए वेब API को उपयोग करने की अनुमति नहीं देता।

## इसे भी देखें:

1. [Elm की औपचारिक गाइड](https://guide.elm-lang.org/)
3. [File API और FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)