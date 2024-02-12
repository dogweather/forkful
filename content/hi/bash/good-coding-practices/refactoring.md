---
title:                "कोड सुधार"
aliases:
- /hi/bash/refactoring/
date:                  2024-01-26T01:17:57.991972-07:00
model:                 gpt-4-0125-preview
simple_title:         "कोड सुधार"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/refactoring.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रीफैक्टरिंग किसी मौजूदा कंप्यूटर कोड की पुनररचना की प्रक्रिया है जिसमें इसके बाह्य व्यवहार को बदले बिना ही संरचनात्मक परिवर्तन किए जाते हैं। यह जटिलता को कम करने, रखरखाव में सुधार और आपके कोडबेस को स्वस्थ और वर्तमान एवं भविष्य के डेवलपर्स के लिए अधिक समझने में आसान बनाने के लिए एक महत्वपूर्ण अभ्यास है।

## कैसे करें:
आइए एक सरल Bash स्क्रिप्ट पर विचार करते हैं जिसे कुछ रीफैक्टरिंग की आवश्यकता है। यह अकड़ा हुआ है, दोहराए गए कोड के साथ और इसका अनुसरण करना कठिन है:

```Bash
#!/bin/bash
echo "Enter a filename:"
read filename
if [ -f "$filename" ]; then
    echo "File exists."
    count=$(grep -c "foo" "$filename")
    echo "The word foo appears $count times."
else
    echo "File does not exist."
fi
```

स्पष्टता और पुन: उपयोगिता के लिए रीफैक्टरिंग में फंक्शन पेश करना और त्रुटियों को अधिक कृपया संभालना शामिल हो सकता है:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Enter a filename:"
    read -r filename
    echo "Enter the word to search for:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "The word $word appears $count times."
    else
        echo "File does not exist." >&2
        exit 1
    fi
}

main "$@"
```

रीफैक्टर किया गया संस्करण पठनीयता में सुधार और संभावित पुन: उपयोग को सक्षम बनाने के लिए फंक्शन का उपयोग करता है।

## गहराई से विचार:
रीफैक्टरिंग कोई ऐसी अवधारणा नहीं है जो Bash या यहां तक कि उच्च-स्तरीय प्रोग्रामिंग भाषाओं के साथ शुरू हुई थी; यह प्रोग्रामिंग के साथ ही उतनी पुरानी है। इस शब्द को 1999 में मार्टिन फॉवलर द्वारा "रीफैक्टरिंग: मौजूदा कोड की डिज़ाइन में सुधार" पुस्तक में आधिकारिक रूप दिया गया था, जिसमें मुख्य रूप से वस्तु-उन्मुख भाषाओं पर ध्यान केंद्रित किया गया था।

Bash स्क्रिप्टिंग के संदर्भ में, रीफैक्टरिंग अक्सर लंबी स्क्रिप्ट्स को फंक्शन्स में तोड़ने, लूप्स या कंडीशनल्स के साथ पुनरावृत्ति को कम करने, और फाइलनेम में व्हाइटस्पेस को संभालने में विफल रहने जैसी सामान्य चूकों से बचने का मतलब रखता है। जिन स्क्रिप्ट्स की जटिलता अधिक हो गई है, उनके लिए Bash के विकल्पों में Python या Perl शामिल हैं, जो जटिल कार्यों के लिए बेहतर डाटा संरचनाएं और त्रुटि संभाल की पेशकश करते हैं।

Bash-विशिष्ट रीफैक्टरिंग अधिकतर सर्वोत्तम प्रथाओं का पालन करने जैसी बातों पर आधारित होती है, जैसे कि चरों को उद्धृत करना, टेस्ट के लिए `[ ]` के बजाय `[[ ]]` का उपयोग करना, और रोबस्ट आउटपुट के लिए `echo` की तुलना में `printf` को प्राथमिकता देना। क्रियान्वयन विवरण अक्सर शैली निर्देशिकाओं का पालन करने और सामान्य गलतियों को पकड़ने के लिए `shellcheck` जैसे उपकरणों का उपयोग करने के आसपास घूमते हैं।

## देखें भी:
- [Google की Shell Style Guide](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, एक shell स्क्रिप्ट्स के लिए स्थैतिक विश्लेषण उपकरण](https://www.shellcheck.net/)
- [The Art of Command Line](https://github.com/jlevy/the-art-of-command-line)
