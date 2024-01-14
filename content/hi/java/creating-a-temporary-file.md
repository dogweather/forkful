---
title:                "Java: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि टेम्पररी फाइल बनाना क्यों जरूरी हो सकता है? शायद आपको यह जानने की जरूरत हो सकती है कि प्रोग्राम को कैसे सशक्त और सुरक्षित बनाया जा सकता है। टेम्पररी फाइल एक अस्थायी स्थान है जहां प्रोग्राम अपने डेटा को संचित कर सकता है। इससे स्थायित्व की समस्याओं से बचा जा सकता है और उपयोगकर्ता का डेटा सुरक्षित रहता है।

## कैसे करें

```Java
try {
  // नई टेम्पररी फाइल बनाएं
  File tempFile = File.createTempFile("temp", ".txt");

  // फाइल पथ प्रिंट करें
  System.out.println("टेम्पररी फाइल का पथ है: " + tempFile.getAbsolutePath());

  // फाइल में डेटा लिखें
  FileWriter writer = new FileWriter(tempFile);
  writer.write("यह एक टेम्पररी फाइल है");
  writer.close();

  // डेटा पढ़ें
  Scanner scanner = new Scanner(tempFile);
  while (scanner.hasNextLine()) {
    String data = scanner.nextLine();
    System.out.println("फाइल से पढ़ा गया डेटा है: " + data);
  }
  scanner.close();

  // फाइल हटाएं
  tempFile.delete();

} catch (IOException e) {
  e.printStackTrace();
}
```

इस उदाहरण में, हमने एक नई टेम्पररी फाइल बनाई है और उसमें कुछ डेटा लिखा है। फिर हमने उस फाइल से डेटा पढ़ा और फाइल को हटा दिया है। यह एक आसान और सुरक्षित तरीका है अस्थायी डेटा को संचित करने का।

## गहराई में जाएं

अगर हम Java में File.createTempFile() फंक्शन की गहराई पर जाएं, तो हम देखेंगे कि यह दो स्टैटिक मेथड्स है: createTempFile(String prefix, String suffix) और createTempFile(String prefix, String suffix, File directory)। पहला मेथड एक अस्थायी फाइल बनाता है और दूसरा एक अस्थायी फाइल झरने का निर्देश