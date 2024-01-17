---
title:                "स्टैंडर्ड त्रुटि पर लेखन"
html_title:           "Javascript: स्टैंडर्ड त्रुटि पर लेखन"
simple_title:         "स्टैंडर्ड त्रुटि पर लेखन"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Since we have been programming, we have been writing code to display output to the user. But sometimes, we also need to display error messages for debugging purposes. To do this, we use standard error in Javascript. It is a stream that displays error messages instead of regular output. Programmers use it to troubleshoot and fix issues in their code.

## कैसे करें:
```Javascript
console.error("This is an error message");
```
इस आसान syntax के प्रयोग से, हम एक error message standard error में दिखा सकते हैं। यह आपको प्रोग्रामिंग में त्रुटियों को सही करने में मदद करेगा।

## गहराई में जाएं:
(1) ऐतिहासिक संदर्भ: स्टैंडर्ड एरर को बनाने की जरूरत बड़े प्रोग्राम्स को ठीक करने के लिए थी। पहले, इस्तेमाल किए जाने वाले operating systems के पास ये सुविधा नहीं थी और उन्हें अपने error messages को फाइल में लिखने की आवश्यकता पड़ती थी। (2) वैकल्पिक: आप console.log() का प्रयोग भी कर सकते हैं, लेकिन ये समय ले सकता है और debugging को कठिन बना सकता है। (3) अनुमेशन विवरण: standard error का उपयोग node.js में process.stderr, ब्राउज़र में console.error(), और इलेक्ट्रॉनिक कंप्यूटर में stderr के रूप में किया जाता है।

## देखें भी:
- [Javascript Console Methods](https://www.w3schools.com/js/js_console.asp)
- [Debugging in Javascript](https://www.freecodecamp.org/news/how-to-debug-in-javascript/)
- [Node.js Process Module](https://nodejs.org/api/process.html)