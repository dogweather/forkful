---
title:                "लॉगिंग"
date:                  2024-03-08T21:55:48.561032-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Dart में लॉगिंग का मतलब है कार्यक्रम के निष्पादन के दौरान विभिन्न स्तरों की जानकारी को रिकॉर्ड करना। प्रोग्रामर्स इसे सॉफ्टवेयर के व्यवहार की निगरानी, समस्याओं का निदान, और प्रदर्शन का विश्लेषण करने के लिए करते हैं, जिससे आवेदन को समय के साथ बनाए रखना और सुधारना आसान हो जाता है।

## कैसे करें:

Dart `dart:developer` लाइब्रेरी के माध्यम से एक साधारण लॉगिंग तंत्र शामिल करता है। अधिक परिष्कृत लॉगिंग की जरूरतों के लिए, प्रोग्रामर्स अक्सर थर्ड-पार्टी लाइब्रेरीज जैसे कि `logger` और `log4dart` की ओर रुख करते हैं।

### `dart:developer` का उपयोग करना
यह मूल लॉगिंग के लिए उपयुक्त है, विशेषकर विकास के दौरान:

```dart
import 'dart:developer';

void main() {
  log('यह एक डीबग लॉग संदेश है।');
}
```

आउटपुट:
```
यह एक डीबग लॉग संदेश है।
```

### `logger` पैकेज का उपयोग करना
एक अधिक सम्पूर्ण समाधान के लिए, `logger` पैकेज विभिन्न स्तरों की लॉगिंग (जैसे कि info, warning, error) प्रदान करता है और इसे अधिक पठनीय तरीके से फॉर्मेट किया जा सकता है।

पहले, अपनी `pubspec.yaml` फाइल में `logger` निर्भरता जोड़ें:

```yaml
dependencies:
  logger: ^1.0.0
```

फिर, इस प्रकार उपयोग करें:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("यह एक डीबग संदेश है");
  logger.w("यह एक चेतावनी संदेश है");
  logger.e("यह एक त्रुटि संदेश है");
}
```

नमूना आउटपुट इस प्रकार दिख सकता है, प्रत्येक संदेश प्रकार को आसान पहचान के लिए अलग ढंग से फॉर्मेट किया जाता है:

```
💬 यह एक डीबग संदेश है
⚠️ यह एक चेतावनी संदेश है
❗️ यह एक त्रुटि संदेश है
```

### `log4dart` पैकेज का उपयोग करना
कॉन्फिग्यूरेशन-आधारित लॉगिंग की आवश्यकता वाले अनुप्रयोगों के लिए (Log4j के समान), `log4dart` एक परिचित दृष्टिकोण प्रदान करता है। यह विशेष रूप से बड़े पैमाने के अनुप्रयोगों के लिए सुविधाजनक है।

सुनिश्चित करें कि आप अपनी `pubspec.yaml` में `log4dart` शामिल करें:

```yaml
dependencies:
  log4dart: ^2.0.0
```

एक साधारण उपयोग उदाहरण:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("MyApp को डिबग करना");
  logger.info("सूचनात्मक संदेश");
}
```

आउटपुट:

```
DEBUG: MyApp को डिबग करना
INFO: सूचनात्मक संदेश
```

ये प्रत्येक तरीके विभिन्न स्तरों की लचीलापन और जटिलता प्रदान करते हैं, सरल डिबगिंग संदेशों से लेकर जटिल अनुप्रयोगों की जरूरतों के अनुरूप व्यापक, कॉन्फिगरेबल लॉगिंग तक।