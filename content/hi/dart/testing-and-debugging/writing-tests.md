---
title:                "टेस्ट लिखना"
date:                  2024-03-08T21:58:50.473415-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Dart में परीक्षण लिखना आपके कार्यक्रम के विभिन्न भागों को स्वचालित रूप से सत्यापित करने के लिए परीक्षा मामलों का निर्माण करना शामिल है। प्रोग्रामर इसे अपने कोड को विश्वसनीय और दोषों से मुक्त बनाने के लिए करते हैं, जिससे आसान अपडेट और पुनर्गठन सुविधाजनक होता है और प्रतिगमन से बचाव होता है।

## कैसे:

Dart में, `test` पैकेज का उपयोग परीक्षण लिखने के लिए आमतौर पर किया जाता है। पहले, अपने `pubspec.yaml` में `test` पैकेज जोड़ें:

```yaml
dev_dependencies:
  test: ^1.0.0
```

फिर, एक सरल फ़ंक्शन के लिए एक परीक्षा लिखें। मान लीजिए आपके पास दो संख्याओं को जोड़ने वाला एक फ़ंक्शन है:

```dart
int add(int a, int b) {
  return a + b;
}
```

अगला, `test` निर्देशिका में `add_test.dart` नामक एक फ़ाइल बनाएं और अपना परीक्षा मामला लिखें:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // मान लीजिए आपका `add` फ़ंक्शन lib/add.dart में है

void main() {
  test('adds two numbers', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

परीक्षणों को चलाने के लिए, Dart कमांड का उपयोग करें:

```bash
$ dart test
```

नमूना आउटपुट इस प्रकार हो सकता है:

```
00:01 +1: All tests passed!
```

### एक तृतीय-पक्ष पुस्तकालय का उपयोग करना: मॉकिंग के लिए Mockito

जटिल निर्भरताओं वाले कोड का परीक्षण करते समय, आप मॉक ऑब्जेक्ट्स बनाने के लिए Mockito का उपयोग कर सकते हैं। पहले, अपने `pubspec.yaml` में Mockito जोड़ें:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

मान लें आपके पास `UserRepository` नामक एक क्लास है जो उपयोगकर्ता डेटा प्राप्त करती है, और आप एक वास्तविक डेटाबेस को हिट किए बिना `UserRepository` पर निर्भर `UserService` का परीक्षण करना चाहते हैं:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Mockito का उपयोग करके एक Mock क्लास बनाएं
class MockUserRepository is Mock implements UserRepository {}

void main() {
  group('UserService Tests', () {
    test('Fetches user successfully', () {
      // Mock instance बनाएं
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Mock व्यवहार सेटअप करें
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Test User'));

      // Assert करें कि मॉक किए गए तरीके को अपेक्षित तर्कों के साथ बुलाया गया है
      expect(userService.getUserName(1), 'Test User');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

यह परीक्षण चलाना पुष्टि करता है कि `UserService` सही ढंग से `UserRepository` के साथ इंटरैक्ट करती है, मॉकिंग का उपयोग करके नियंत्रित तरीके से वास्तविक इंटरेक्शन्स का अनुकरण करती है।
