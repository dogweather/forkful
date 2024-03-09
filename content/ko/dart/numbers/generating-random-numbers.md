---
title:                "임의의 숫자 생성하기"
date:                  2024-03-08T21:54:42.220923-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
Dart에서 난수를 생성하는 것은 예측할 수 없으며 각 실행마다 다른 숫자 값을 만드는 것을 말합니다. 프로그래머들은 테스트 환경에서 실제 상황을 시뮬레이션하는 것부터 게임 메커니즘을 활성화하고 암호화 작업에서 무작위성을 통해 보안을 보장하는 것까지 다양한 이유로 이 기능을 활용합니다.

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
