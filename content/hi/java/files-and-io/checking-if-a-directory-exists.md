---
title:                "डायरेक्टरी मौजूद है या नहीं जाँचना"
aliases: - /hi/java/checking-if-a-directory-exists.md
date:                  2024-02-03T19:09:22.945722-07:00
model:                 gpt-4-0125-preview
simple_title:         "डायरेक्टरी मौजूद है या नहीं जाँचना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
जावा में यह जांचना कि कोई डायरेक्टरी मौजूद है या नहीं, एक मौलिक कार्य है जो इसे पढ़ने, इसमें लिखने, या उसके अस्तित्व की आवश्यकता वाले किसी भी ऑपरेशन को प्रदर्शन से पहले फाइल सिस्टम निर्देशिका की उपस्थिति की पुष्टि करने की प्रक्रिया है। यह फ़ाइल सिस्टम के साथ बातचीत करने वाले प्रोग्रामों में त्रुटियों या अपवादों से बचने के लिए महत्वपूर्ण है, जिससे समर्थन और बेहतर उपयोगकर्ता अनुभव सुनिश्चित होता है।

## कैसे करें:
जावा में, किसी डायरेक्टरी के अस्तित्व की जाँच करने के कई तरीके हैं, मुख्यतः `java.nio.file.Files` और `java.io.File` कक्षाओं का उपयोग करके।

**`java.nio.file.Files` का उपयोग करते हुए**:

यह हाल के जावा संस्करणों में अनुशंसित दृष्टिकोण है।

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // यहाँ डायरेक्टरी का पथ निर्दिष्ट करें
        String directoryPath = "path/to/directory";

        // यह जांचना कि डायरेक्टरी मौजूद है या नहीं
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("डायरेक्टरी मौजूद है।");
        } else {
            System.out.println("डायरेक्टरी मौजूद नहीं है।");
        }
    }
}
```
**नमूना आउटपुट**:
```
डायरेक्टरी मौजूद है।
```
या
```
डायरेक्टरी मौजूद नहीं है।
```

**`java.io.File` का उपयोग करते हुए**:

हालांकि `java.nio.file.Files` की सिफारिश की जाती है, पुरानी `java.io.File` कक्षा का भी उपयोग किया जा सकता है।

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // यहाँ डायरेक्टरी का पथ निर्दिष्ट करें
        String directoryPath = "path/to/directory";

        // फाइल ऑब्जेक्ट बनाना
        File directory = new File(directoryPath);

        // यह जांचना कि डायरेक्टरी मौजूद है या नहीं
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("डायरेक्टरी मौजूद है।");
        } else {
            System.out.println("डायरेक्टरी मौजूद नहीं है।");
        }
    }
}
```
**नमूना आउटपुट**:
```
डायरेक्टरी मौजूद है।
```
या
```
डायरेक्टरी मौजूद नहीं है।
```

**तृतीय-पक्ष पुस्तकालयों का उपयोग करना**:

हालांकि इस कार्य के लिए मानक जावा पुस्तकालय आमतौर पर पर्याप्त होता है, Apache Commons IO जैसे तृतीय-पक्ष पुस्तकालय अधिक जटिल अनुप्रयोगों में उपयोगी हो सकते हैं।

**Apache Commons IO**:

सबसे पहले, अपने प्रोजेक्ट में Apache Commons IO निर्भरता को जोड़ें। फिर, आप एक डायरेक्टरी के अस्तित्व की जाँच करने के लिए इसकी सुविधाओं का उपयोग कर सकते हैं।

```java
// मान लें कि प्रोजेक्ट में Apache Commons IO जोड़ा गया है

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // यहाँ डायरेक्टरी का पथ निर्दिष्ट करें
        String directoryPath = "path/to/directory";

        // FileUtils का उपयोग करके जांचना
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("डायरेक्टरी मौजूद है।");
        } else {
            System.out.println("डायरेक्टरी मौजूद नहीं है।");
        }
    }
}
```

**नोट**: `FileUtils.directoryContains` जांचता है कि कोई डायरेक्टरी विशिष्ट फ़ाइल को समेटे हुए है या नहीं, परन्तु दूसरे तर्क के रूप में `null` पास करके, आप इसे डायरेक्टरी के अस्तित्व की जांच के लिए उपयोग कर सकते हैं। सावधान रहें, क्योंकि यह विधि का सबसे सीधा या इरादा किया गया उपयोग नहीं हो सकता है।

**नमूना आउटपुट**:
```
डायरेक्टरी मौजूद है।
```
या
```
डायरेक्टरी मौजूद नहीं है।
```
