---
title:                "C: टेक्स्ट खोजना और प्रतिस्थापन करना"
simple_title:         "टेक्स्ट खोजना और प्रतिस्थापन करना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी टेक्स्ट में एक शब्द को दूसरे शब्द से बदलने की आवश्यकता महसूस की है? कई बार इस समस्या का सामना करने के लिए आपको पूरे दस्तावेज़ों या कोड फाइलों को नए शब्दों से बदलने की ज़रूरत हो सकती है। लेकिन क्या आपने कभी सोचा है कि ऐसे काम को कितना आसान बनाना हो सकता है? यह सब करने के लिए आपको सिर्फ एक छोटा सा कोड लिखना होगा जिसे हम 'search and replace' कहते हैं। अगर आप C प्रोग्रामिंग समझते हैं तो इस तरह के कोड लिखना आपके लिए बहुत ही आसान हो सकता है।

## कैसे करें

यदि आपने कभी पहले से कोई प्रोग्रामिंग भाषा जैसे C में कोड लिखा है तो आप search and replace के बारे में बहुत सीख चुके होंगे। हालांकि अगर आप नए हैं, तो आपको चिंता करने की ज़रूरत नहीं है। आपको सिर्फ इस प्रकार का छोटा सा कोड लिखना होगा जो हमारे example.c नाम के फाइल में दिए गए है।

```C
#include <stdio.h>

int main() {

    // Initializing variables
    char text[] = "मैं एक प्रोग्रामर हूं";
    char find[] = "प्रोग्रामर";
    char replace[] = "नाज़दीकी दोस्त";

    // Using a loop to search and replace
    for (int i = 0; text[i] != '\0'; i++) {

        // Checking if current character matches with the first character of find string
        if (text[i] == find[0]) {

            int j, start = i, found = 1;

            // Checking if all characters in find match with the text
            for (j = 0; find[j] != '\0'; j++) {

                // If mismatch is found, set found flag to false and break the loop
                if (text[i] != find[j]) {
                    found = 0;
                    break;
                }

                i++; // Incrementing i to match with next character of find
            }

            // If found flag is true, replace the matched characters with replace string
            if (found == 1) {
                for (j = start; find[j - start] != '\0'; j++)
                    text[j] = replace[j - start];

                i = start + j - start - 1; // Updating i to new position after the