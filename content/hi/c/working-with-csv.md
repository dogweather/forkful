---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV यानी 'Comma-Separated Values' एक साधारण फाइल फॉर्मेट है जो डेटा को सादे पाठ (plain text) में स्टोर करता है, प्रत्येक रिकॉर्ड को कॉमा से अलग करते हुए। प्रोग्रामर्स डेटा एक्सचेंज, डेटाबेस इंपोर्ट/एक्सपोर्ट और डेटा एनालिसिस के लिए CSV का उपयोग करते हैं।

## How to (कैसे करें):
नीचे C कोड का उदाहरण है जो CSV फाइल पढ़ता है:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp = fopen("example.csv", "r");
    if (!fp) {
        printf("फाइल खोलने में असफल हुए।\n");
        return EXIT_FAILURE;
    }

    char buffer[1024];
    int row = 0;
    int column;

    while (fgets(buffer, 1024, fp)) {
        column = 0;
        row++;
        char *value = strtok(buffer, ",");
        while (value) {
            printf("%d रो: %d कॉलम: %s\n", row, ++column, value);
            value = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```

सैंपल CSV फाइल (`example.csv`):

```
नाम,उम्र,शहर
राम,25,दिल्ली
सीता,22,मुंबई
```

आउटपुट:

```
1 रो: 1 कॉलम: नाम
1 रो: 2 कॉलम: उम्र
1 रो: 3 कॉलम: शहर
2 रो: 1 कॉलम: राम
2 रो: 2 कॉलम: 25
2 रो: 3 कॉलम: दिल्ली
3 रो: 1 कॉलम: सीता
3 रो: 2 कॉलम: 22
3 रो: 3 कॉलम: मुंबई
```

## Deep Dive (गहराई से जानकारी):
CSV का प्रयोग 1970 के दशक से शुरू हुआ और यह सबसे सरल टेक्स्ट-बेस्ड डेटा फॉर्मेट में से एक है। जहां CSV फाइल्स सादगी प्रदान करते हैं, वहीं JSON या XML जैसे वैकल्पिक फॉर्मेट्स अधिक संरचित और कॉम्प्लेक्स डेटा को स्टोर करने में बेहतर हो सकते हैं। C प्रोग्रामिंग में CSV को हैंडल करते समय आपको फाइल I/O और स्ट्रिंग मैनिपुलेशन की अच्छी समझ होनी चाहिए, और साथ ही मेमोरी मैनेजमेंट का भी ध्यान रखना पड़ता है।

## See Also (और जानिए):
- C Programming Language: https://en.wikipedia.org/wiki/C_(programming_language)
- CSV पर RFC 4180 स्टैंडर्ड: https://tools.ietf.org/html/rfc4180
- डेटा पारसिंग के लिए C लाइब्रेरीज: http://libcsv.sourceforge.net/
