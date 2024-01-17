---
title:                "टेक्स्ट फाइल पढ़ना"
html_title:           "Gleam: टेक्स्ट फाइल पढ़ना"
simple_title:         "टेक्स्ट फाइल पढ़ना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## एकीकृत क्या है और क्यों?
एकीकृत (Gleam) में एक पाठ फ़ाइल को पढ़ना एक आम प्रोग्रामिंग कार्य है जो एक सदस्य चर सूची (list of chars) को पढ़ता है जिसमें प्रचलन (current) प्रोग्राम इनफ़ार्मेशन (program information) को भी शामिल किया जाता है। इसके आधार पर, प्रोग्रामरशिप (programming) कई उदाहरण से अधिक जानकारी को कूटबन्दी (retrieve) करता है जिसकी आवश्यकता उसी समय में पैदा होती है जब एकत्रित (gather) अंकिकों (numbers) को अधिक समझने की आवश्यकता होती है। चलें, हम पाठ फ़ाइलों को कैसे पढ़ते हैं।

## कैसे:
पाठ फ़ाइल हासिल करने की सबसे आसान (easy) तकनीक (technique) पैरामीटर के साथ ```Gleam.File.read("filename.txt")``` चहे। ```filename.txt``` को अपनी फ़ाइल का नाम बदल दें। अब अपने वर्ग (class) में आप पाठ फ़ाइल के सभी लाइनों (lines) को प्रिंट (print) कर सकते हैं: 

```Gleam
use gleam/file

fn read_file(file_name) {
    let result = File.read(file_name)
    case result {
        ok(file_contents) -> List.map(print, file_contents)
        _ -> ()
    }
}
```

Output:
    
```
> read_file("filename.txt") 
line 1
line 2
line 3
```

## गहराई तक:
पाठ फ़ाइलों को पढ़ने के अलावा, प्रोग्रामर्स अन्य कई तकनीकों (techniques) का उपयोग कर सकते हैं जैसे कि पाठ फ़ाइलें लिखना, पाठ फ़ाइलें कैच (cache) करना, और तकनीकी प्रणालियों (operating systems) में लेखन-घटकों (writing components) को सक्षम (enable) करना। इस तरह की संरचनाएं आपको परिचित (familiar) हो सकती हैं और आप अपने प्रोजेक्ट (projects) में उन्हें उपयोग कर सकते हैं। पाठ फ़ाइलों को पढ़ने का यह प्रकार आपको अन्य प्रोग्रामिंग भाषाओं में भी मिल सकती है उदा.प. Java में ```FileReader``` क्लास शामिल है जो एक सीधा रूप से फ़ाइल अधि (stream) को पढ़ने के लिए उपयोग किया जाता है, रोजाना प्रोग्रामर्शिप (programming) दुनिया में जो वहां खुला है। पाठ फ़ाइल / स्ट्रिंग्स की तकनीकों का समझ उचित और अपने को बेहतर प्रोग्रामर के रूप में शामिल करने का सबसे आसान तरीका है।

## देखें भी:
- [Gleam.FIle](https://gleam.run/modules/gleam/file.html) अनुसारक
- [तकनीकी जानकारी चाहे तो।](https://en.wikipedia.org/wiki/Text_file) (इंग्लिश)