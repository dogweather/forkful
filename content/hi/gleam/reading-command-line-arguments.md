---
title:                "Gleam: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों
कुछ समझोगे तो समझेंगे

## कैसे करे
command line arguments बनाओ लेकिन कैसे. नीचे दी गयी कोडिंग उदाहरण और सैंपल output देखो "```Gleam ...```" कूट्स में:

```Gleam
import gleam/io

fn main() {
  // अगर कोई भी command line arguments दिये गए हैं, उन्हें देखा जायेगा
  // लेकिन अगर कोई खाली h तब संदेश देखो
  case io.args() {
    // io.args() में से पहला हटाओ क्योंकि ये फ़ाइल नाम हो सकता है
    [_, h | _] -> io.println("हेलो " ++ h)
    _ -> io.println("हेलो world")
  }
}
```

यदि आप इसको compile और run करते हैं, तो आप नीचे के सैंपल output को देखेंगे:

```
$ gleam run hello_world.gleam
अगर आप hello_world.gleam नाम के फ़ाइल को compile और run करते हैं

hello world

$ gleam run hello_world.gleam Sebastian
यदि आप अपना खुदका नाम देते हैं

hello Sebastian
```

## डीप डाइव
command line arguments पाठ्यक्रम के साथ दिए गए स्वरूप को अन्य तरीके से प्रदर्शित करता है जो आपको इससे ज्यादा बुनियादी जानकारी प्रदान करता है। आप इसे परिवर्तित कर सकते हैं और इस कार्यों को अनिवार्य रूप से, उदाहरण के लिए, सुनिश्चित कर सकते हैं कि किस command line argument आपको कौन से कार्यों के साथ जुड़े हैं।

आप हमारे [documentation](https://gleam.run/book/tutorials/command_line_arguments.html) पर जा कर गहराई से command line arguments के बारे में पढ़ सकते हैं।

## देखिये
[मार्कडाउन और ग्लीम](https://gleam.run/book/tutorials/documentation.html)
[ग्लीम से सीखिए प्रोग्रामिंग](https://gleam.run/book/)