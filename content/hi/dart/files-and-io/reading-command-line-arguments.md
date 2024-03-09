---
title:                "कमांड लाइन तर्कों को पढ़ना"
date:                  2024-03-08T21:57:24.621968-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Dart में कमांड लाइन तर्कों (arguments) को पढ़ना प्रोग्रामरों को एक Dart कार्यक्रम को निष्पादित करते समय सीधे कंसोल में डेटा इनपुट करने की अनुमति देता है, जिससे इसकी इंटरएक्टिविटी और विविध उपयोग के मामलों के लिए लचीलापन बढ़ता है, जिसमें ऑटोमेशन स्क्रिप्ट्स, CLI टूल्स, या बैच प्रोसेसिंग शामिल हैं। यह सुविधा कमांड-लाइन एप्लीकेशंस को अनुकूलनीय और उपयोगकर्ता-मित्रता बनाने के लिए महत्वपूर्ण है।

## कैसे करें:

Dart मुख्य विधि में `List<String> args` के माध्यम से कमांड लाइन तर्कों तक पहुंचने का एक सरल दृष्टिकोण प्रदान करता है। नीचे एक सरल उदाहरण दिया गया है जो दिखाता है कि कैसे कमांड लाइन तर्कों को पढ़ा और उपयोग किया जाता है।

```dart
// main.dart
void main(List<String> args) {
  print('Command Line Arguments:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

इस Dart कार्यक्रम को चलाने और कमांड लाइन तर्कों को पास करने के लिए, Dart CLI का इस प्रकार उपयोग करें:

```shell
dart run main.dart Hello World!
```

अपेक्षित आउटपुट:

```
Command Line Arguments:
1: Hello
2: World!
```

### एक लोकप्रिय थर्ड-पार्टी लाइब्रेरी का उपयोग करना: `args`

हालांकि Dart की अंतर्निहित क्षमताएं कई एप्लिकेशंस के लिए कमांड लाइन तर्कों को संभालने के लिए मजबूत हैं, `args` पैकेज अधिक जटिल आवश्यकताओं के लिए कमांड लाइन तर्कों को परिभाषित करने और पार्स करने का एक सुधारित तरीका प्रदान करता है।

सबसे पहले, `args` पैकेज को आपके `pubspec.yaml` में जोड़ें:

```yaml
dependencies:
  args: ^2.0.0
```

फिर, इसे अपने कार्यक्रम में इस प्रकार उपयोग करें:

```dart
// Using the 'args' package
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Hello, ${argResults['name']}!');
  } else {
    print('No name provided.');
  }
}
```

नामित तर्क के साथ कार्यक्रम चलाएँ:

```shell
dart run main.dart --name=John
```

अपेक्षित आउटपुट:

```
Hello, John!
```

कमांड लाइन तर्कों को पार्स करने का यह सरल परिचय, दोनों देशी और `args` लाइब्रेरी के साथ, दर्शाता है कि कैसे Dart कंसोल से सीधे उपयोगकर्ता इनपुट्स को संभाल सकता है, जिससे अधिक इंटरएक्टिव और डायनामिक CLI एप्लिकेशंस बनाने का मार्ग प्रशस्त होता है।
