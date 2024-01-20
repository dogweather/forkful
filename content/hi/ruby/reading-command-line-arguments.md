---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Ruby में Command Line Arguments का उपयोग कैसे करें?

## क्या और क्यों?

Command line arguments Ruby प्रोग्राम के माध्यम से आपके script को चलाते समय लिया जाने वाले आदेश हैं। इसे करने का मुख्य कारण यह है कि यह programmer को अपने code की flexibility और अधिक customization की अनुमति देता है।

## कैसे करें:

Ruby में command line arguments को `ARGV` विशेष array का उपयोग करके पढ़ सकते हैं।

```Ruby
# test.rb script
ARGV.each do|a|
  puts "Argument: #{a}"
end
```

शुरू करने के लिए, अपने Terminal में निम्नलिखित command चलाएं:

```sh
ruby test.rb हेलो वर्ल्ड
```

आपकी output इन लाइनों के बराबर होगी:

```
Argument: हेलो
Argument: वर्ल्ड
```

## Deep Dive

Command line arguments का सामान्य उपयोग उन scripts के लिए होता है जो बार-बार चलाए जा सकते हैं और विभिन्न parameters के आधार पर विभिन्न results उत्पन्न कर सकते हैं।

ऐतिहासिक दृष्टिकोण से, command line argument का कार्य किसी भी प्रोग्राम को चलाने के लिए inline inputs की आवश्यकता को कम करता है।

एक विकल्प command line option parsing libraries, जैसे कि `OptionParser` या `Thor`. इन libraries का उपयोग उन switches और flags के लिए किया जाता है जिनकी आपको आवश्यकता हो सकती है, उदाहरण स्वरूप '-v' या '--versions' के साथ।

## और देखें

अधिक जानकारी के लिए, निम्न लिंकों पर जाएं:

1. Ruby Docs on Command Line Arguments: [https://docs.ruby-lang.org/en/2.1.0/ARGF.html](https://docs.ruby-lang.org/en/2.1.0/ARGF.html)
2. Stack Overflow discussion on ARGV: [https://stackoverflow.com/questions/9861301/what-is-argv-in-ruby](https://stackoverflow.com/questions/9861301/what-is-argv-in-ruby)