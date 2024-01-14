---
title:                "C: एक टेक्स्ट फ़ाइल लिखना"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

हम जब भी कोई पाठ फ़ाइल बनाते हैं, तो उसके पीछे कुछ वजह होती है। शायद हमें अपनी डेटा को सुरक्षित रखने की जरूरत हो, या फिर उसे साझा करने के लिए। इसलिए इस भाषा में हम जानेंगे कि एक पाठ फ़ाइल क्यों आवश्यक हो सकती है।

## कैसे करें

अब जब हमने जान लिया है कि पाठ फ़ाइल क्यों लिखी जाती है, तो आइए अब देखें कि इसे कैसे लिखा जाता है। नीचे दिए गए कोड ब्लॉक में हम C भाषा में पाठ फ़ाइल लिखने का एक उदाहरण देखेंगे।

```C
#include <stdio.h>

int main()
{
  FILE *fp;
  char text[100];

  // Creating file pointer and opening file in write mode
  fp = fopen("text_file.txt", "w");

  // Getting input from user
  printf("Enter some text: ");
  gets(text);

  // Writing input to file
  fprintf(fp, "%s", text);

  // Closing file
  fclose(fp);

  // Printing success message
  printf("Text file created successfully!");

  return 0;
}
```

उपरोक्त कोड प्रोग्राम को Compile और Execute करने पर यह एक `text_file.txt` नाम की पाठ फ़ाइल बनाएगा और उसमें आपके द्वारा दिए गए text को लिखेगा। हम देख सकते हैं कि `fopen()` और `fprintf()` फ़ंक्शन के द्वारा हम पाठ फ़ाइल लिख सकते हैं।

## गहराई से जाने

अब आपने एक बुनियादी स्तर पर जान लिया है कि पाठ फ़ाइल क्या होती है और इसे कैसे लिखा जाता है। लेकिन इस विषय में और गहराई से जानने के लिए, आप फ़ाइल लिखने के अन्य तरीके जैसे `fprintf()` के साथ `fputs()` और `fputc()` भी जान सकते हैं। इसके अलावा, आप फ़ाइल को खोलने के तरीके भी पढ़ सकते हैं जैसे `r` (read), `a` (append)