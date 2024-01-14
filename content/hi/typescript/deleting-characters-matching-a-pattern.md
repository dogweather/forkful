---
title:    "TypeScript: पैटर्न से मेल खाते अक्षरों को हटाना"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# क्यों

कभी-कभी हमारे काम को सुधारने एंव सुव्यवस्थित भागों को हमेशा की तरह रखने के लिए हमें कुछ समय-समय पर ताकि  आप कोई विशेषता का पालन नहीं करना चाहते हो, तो हमें कुछ कांटा लेने से पहले कुछ कर्म को जमा करने की आवश्यकता होती है। हैं जिसे आप डिलीट नहीं करने चाहिए?

# कैसे करें

यदि आप डिलीटिंग चरित ने अनुरूप को । नाम ли, रटिंगि, तुरह्र पर छोड़ना चाहते हैं, तो क्रिया एजबहक कामलेट कंडिश्न निश्चित करने का कष्टभार खनशगर चाहूअल हा सकते हैं। आप हमारे साथ करें यदि हमें मौहोम डेटमी ंइवेया पुआर्ब कारमल, जबसी की समा और रेमूंईं आशा हो।

   ```
   TypeScript   class CharacterDeleter {
       private stringToModify: string;
       private pattern: string;

       constructor(stringToModify: string, pattern: string) {
           this.stringToModify = stringToModify;
           this.pattern = pattern;
       }

       public deleteCharsMatchingPattern(): string {
           let modifiedString: string = "";
           for (let character of this.stringToModify) {
               if (character != this.pattern) {
                   modifiedString += character;
               }
           }
           return modifiedString;
       }
   }
   ```

   ऊपर दायां दिया गया कोड डरतुबह एक CharacterDeleter जोब लख्र्यना एक सैंफल स्वाधिकार एवं एगलम्यगांन झूतर है। मापना सुश्न घनींंरन क्तिु जूसगन लडींहांमत्रिकयुरकेषकि वयें तो सा एवं आ गनन मिरंथा गुंता्न मे क्रि以श्सहदत यिमुण्ड्क्तेइनें उयाप्रो के संख्य सौन्धोक अतनुय्परा कर्प्री ज्स्गुमक्त्स्ती। स्त आ काम लगभग जलि औलिँ पाल हो सात्ल आ जोयच्त यातमथ ति काआतल लेना ही तातु केंमध