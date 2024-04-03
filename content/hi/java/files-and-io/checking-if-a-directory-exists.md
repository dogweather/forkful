---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:22.945722-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u091C\u093E\u0935\
  \u093E \u092E\u0947\u0902, \u0915\u093F\u0938\u0940 \u0921\u093E\u092F\u0930\u0947\
  \u0915\u094D\u091F\u0930\u0940 \u0915\u0947 \u0905\u0938\u094D\u0924\u093F\u0924\
  \u094D\u0935 \u0915\u0940 \u091C\u093E\u0901\u091A \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0915\u0908 \u0924\u0930\u0940\u0915\u0947 \u0939\u0948\u0902, \u092E\u0941\
  \u0916\u094D\u092F\u0924\u0903 `java.nio.file.Files` \u0914\u0930 `java.io.File`\
  \ \u0915\u0915\u094D\u0937\u093E\u0913\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0915\u0947\u0964\u2026"
lastmod: '2024-03-13T22:44:52.138673-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u093E\u0935\u093E \u092E\u0947\u0902, \u0915\u093F\u0938\u0940 \u0921\
  \u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u0915\u0947 \u0905\u0938\
  \u094D\u0924\u093F\u0924\u094D\u0935 \u0915\u0940 \u091C\u093E\u0901\u091A \u0915\
  \u0930\u0928\u0947 \u0915\u0947 \u0915\u0908 \u0924\u0930\u0940\u0915\u0947 \u0939\
  \u0948\u0902, \u092E\u0941\u0916\u094D\u092F\u0924\u0903 `java.nio.file.Files` \u0914\
  \u0930 `java.io.File` \u0915\u0915\u094D\u0937\u093E\u0913\u0902 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\u0964\n\n**`java.nio.file.Files`\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\
  \u090F**."
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902 \u091C\u093E\u0901\
  \u091A\u0928\u093E"
weight: 20
---

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
