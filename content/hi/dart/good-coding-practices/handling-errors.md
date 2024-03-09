---
title:                "गलतियों का समाधान"
date:                  2024-03-08T21:56:44.542921-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Dart में त्रुटियों को संभालना कार्यक्रम निष्पादन के दौरान उत्पन्न होने वाली अपवादों की अपेक्षा और प्रबंधित करने के बारे में है ताकि विश्वसनीयता और उपयोगिता में सुधार किया जा सके। प्रोग्रामर दुर्घटनाओं को रोकने और उपयोगकर्ताओं को सार्थक प्रतिक्रिया प्रदान करने के लिए त्रुटि संचालन को लागू करते हैं, जिससे एक चिकनी, सुरक्षित अनुप्रयोग अनुभव सुनिश्चित होता है।

## कैसे:
Dart दो प्रकार की त्रुटियों का समर्थन करता है: *संकलन-समय* त्रुटियाँ और *रन-समय* त्रुटियाँ। संकलन-समय की त्रुटियाँ Dart विश्लेषक द्वारा कोड चलने से पहले पता लगाई जाती हैं, जबकि रन-समय त्रुटियाँ, या अपवाद, निष्पादन के दौरान होती हैं। यहाँ Dart में अपवादों को कैसे संभाला जाता है:

### ट्राई-कैच
अपवादों को कैप्चर करने और अपने अनुप्रयोग को क्रैश होने से रोकने के लिए `try-catch` का उपयोग करें:

```dart
try {
  var result = 100 ~/ 0; // शून्य से विभाजन की कोशिश, एक अपवाद फेंकता है
} catch (e) {
  print('एक अपवाद पकड़ा गया: $e'); // अपवाद को संभालता है
}
```
नमूना आउटपुट: `एक अपवाद पकड़ा गया: IntegerDivisionByZeroException`

### विशेष अपवाद
विशेष अपवादों को संभालने के लिए, `catch` के बाद अपवाद का उल्लेख करें:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('शून्य से विभाजित नहीं कर सकते।'); // विशेष रूप से शून्य विभाजन अपवादों को संभालता है
}
```
नमूना आउटपुट: `शून्य से विभाजित नहीं कर सकते।`

### स्टैक ट्रेस
डिबगिंग के लिए एक स्टैक ट्रेस प्राप्त करने के लिए, कैच ब्लॉक में दूसरा पैरामीटर उपयोग करें:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('अपवाद: $e');
  print('स्टैक ट्रेस: $s'); // डिबगिंग के लिए स्टैक ट्रेस प्रिंट करता है
}
```

### अंत में
त्रुटियों के फेंकने के बावजूद ट्राई/कैच के बाद कोड को निष्पादित करने के लिए `finally` का उपयोग करें:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('एक अपवाद पकड़ा गया: $e');
} finally {
  print('यह हमेशा निष्पादित होता है।'); // सफाई कोड या अंतिम कदम
}
```
नमूना आउटपुट:
```
एक अपवाद पकड़ा गया: IntegerDivisionByZeroException
यह हमेशा निष्पादित होता है।
```

### तृतीय-पक्ष पुस्तकालय
हालांकि Dart की मुख्य पुस्तकालय त्रुटि संचालन के लिए मजबूत है, आप त्रुटि संचालन के लिए `Either` और `Option` जैसी अवधारणाओं को पेश करने वाले `dartz` जैसे कार्यात्मक प्रोग्रामिंग के लिए तृतीय-पक्ष पैकेजों का उपयोग भी कर सकते हैं। `dartz` का उपयोग करके त्रुटि संचालन का एक उदाहरण यहाँ दिया गया है:

1. अपनी `pubspec.yaml` फाइल में निर्भरताओं के अंतर्गत `dartz` को जोड़ें:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. अपने Dart कोड में त्रुटियों को कृपया संभालने के लिए `Either` का उपयोग करें:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('शून्य से विभाजित नहीं कर सकते।');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('त्रुटि: $left'), 
    (right) => print('परिणाम: $right')
  );
}
```
नमूना आउटपुट: `त्रुटि: शून्य से विभाजित नहीं कर सकते।`

`Left` भाग आमतौर पर त्रुटि का प्रतिनिधित्व करता है, और `Right` भाग सफलता का प्रतिनिधित्व करता है। यह पैटर्न एक अधिक कार्यात्मक तरीके से त्रुटियों का संचालन करने की अनुमति देता है, जो त्रुटि प्रबंधन पर स्पष्टता और नियंत्रण प्रदान करता है।