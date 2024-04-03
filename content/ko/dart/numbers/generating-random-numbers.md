---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:42.220923-07:00
description: "\uBC29\uBC95: Dart\uC758 \uCF54\uC5B4 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC5D0\uB294 `dart:math`\uC5D0 \uC788\uB294 `Random` \uD074\uB798\uC2A4\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uB294 \uC9C0\uC6D0\uC774\
  \ \uD3EC\uD568\uB418\uC5B4 \uC788\uC2B5\uB2C8\uB2E4. \uAE30\uBCF8 \uC608\uC81C\uB294\
  \ \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.781176-06:00'
model: gpt-4-0125-preview
summary: "Dart\uC758 \uCF54\uC5B4 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0\uB294 `dart:math`\uC5D0\
  \ \uC788\uB294 `Random` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB09C\
  \uC218\uB97C \uC0DD\uC131\uD558\uB294 \uC9C0\uC6D0\uC774 \uD3EC\uD568\uB418\uC5B4\
  \ \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC784\uC758\uC758 \uC22B\uC790 \uC0DD\uC131\uD558\uAE30"
weight: 12
---

## 방법:
Dart의 코어 라이브러리에는 `dart:math`에 있는 `Random` 클래스를 사용하여 난수를 생성하는 지원이 포함되어 있습니다. 기본 예제는 다음과 같습니다:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // 0과 99 사이의 임의의 정수를 생성
  double randomDouble = rand.nextDouble(); // 0.0과 1.0 사이의 임의의 더블을 생성
  print(randomNumber);
  print(randomDouble);
}
```

*샘플 출력: (실행할 때마다 다릅니다)*

```
23
0.6722390975465775
```

암호학적 난수가 필요한 경우, Dart는 `Random.secure` 생성자를 제공합니다:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*샘플 출력: (실행할 때마다 다릅니다)*

```
45
```

Flutter 프로젝트를 진행하거나 더 복잡한 난수가 필요한 경우, 이름, 주소, 날짜와 같은 다양한 무작위 데이터를 생성하는데 유용한 `faker` 패키지를 찾을 수 있습니다.

`faker`를 사용하려면, 먼저 `pubspec.yaml` 파일에 추가하세요:

```yaml
dependencies:
  faker: ^2.0.0
```

그런 다음, 다음과 같이 가져와서 사용하세요:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // 임의의 이름을 생성
  print(faker.address.city()); // 임의의 도시 이름을 생성
}
```

*샘플 출력:*

```
Josie Runolfsdottir
East Lysanne
```
