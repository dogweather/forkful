---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Command line arguments, कोड को चलाते समय input की तरह काम करते हैं। ये प्रोग्रामर्स को उनके कोड को अधिक flexible बनाने में मदद करते हैं।

## कैसे करें:

आप इसे Node.js के process.argv array का उपयोग करके प्राप्त कर सकते हैं। 

```Javascript
// सभी command line arguments प्रिंट करो
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});
```

 यदि आप निम्नलिखित कोड को `node test.js one two=three four` के साथ चलाते हैं, तो आपको निम्नलिखित आउटपुट मिलेगा:

```Javascript
0: node
1: /path/to/your/test.js
2: one
3: two=three
4: four
```

## गहरी डाइव:

- **ऐतिहासिक प्रक्षेप**: Days of old में, shell scripts और Perl ही command line arguments के साथ काम करने के लिए सबसे पहली भाषाओं में संगनक थीं।
- **वैकल्पिक विधियां**: Yargs, commander, minimist जैसे कई third-party लाइब्ररीज का उपयोग करके भी command line arguments को परिवर्तित किया जा सकता है।
- **विवरण**: Node.js में, process.argv array में command line input को पसंत करने की process का एक विस्तारित विवरण लिखा है।

## देखें भी:

- [Node.js Documentation: Process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [NPM: Yargs](https://www.npmjs.com/package/yargs)
- [NPM: Commander](https://www.npmjs.com/package/commander)
- [NPM: Minimist](https://www.npmjs.com/package/minimist)