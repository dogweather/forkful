---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:01.240991-07:00
description: "Dart\uC5D0\uC11C \uC624\uB958 \uCC98\uB9AC\uB294 \uD504\uB85C\uADF8\uB7A8\
  \ \uC2E4\uD589 \uC911 \uBC1C\uC0DD\uD558\uB294 \uC608\uC678\uB97C \uC608\uC0C1\uD558\
  \uACE0 \uAD00\uB9AC\uD558\uC5EC \uC2E0\uB8B0\uC131\uACFC \uC0AC\uC6A9\uC131\uC744\
  \ \uD5A5\uC0C1\uC2DC\uD0A4\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCDA9\uB3CC\uC744 \uBC29\uC9C0\uD558\
  \uACE0 \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uC758\uBBF8 \uC788\uB294 \uD53C\uB4DC\uBC31\
  \uC744 \uC81C\uACF5\uD558\uC5EC \uB354 \uB9E4\uB044\uB7FD\uACE0 \uC548\uC804\uD55C\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uACBD\uD5D8\uC744 \uBCF4\uC7A5\uD558\uAE30\
  \ \uC704\uD574 \uC624\uB958 \uCC98\uB9AC\uB97C \uAD6C\uD604\uD569\uB2C8\uB2E4."
lastmod: '2024-03-09T21:06:18.755526-07:00'
model: gpt-4-0125-preview
summary: "Dart\uC5D0\uC11C \uC624\uB958 \uCC98\uB9AC\uB294 \uD504\uB85C\uADF8\uB7A8\
  \ \uC2E4\uD589 \uC911 \uBC1C\uC0DD\uD558\uB294 \uC608\uC678\uB97C \uC608\uC0C1\uD558\
  \uACE0 \uAD00\uB9AC\uD558\uC5EC \uC2E0\uB8B0\uC131\uACFC \uC0AC\uC6A9\uC131\uC744\
  \ \uD5A5\uC0C1\uC2DC\uD0A4\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uCDA9\uB3CC\uC744 \uBC29\uC9C0\uD558\
  \uACE0 \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uC758\uBBF8 \uC788\uB294 \uD53C\uB4DC\uBC31\
  \uC744 \uC81C\uACF5\uD558\uC5EC \uB354 \uB9E4\uB044\uB7FD\uACE0 \uC548\uC804\uD55C\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uACBD\uD5D8\uC744 \uBCF4\uC7A5\uD558\uAE30\
  \ \uC704\uD574 \uC624\uB958 \uCC98\uB9AC\uB97C \uAD6C\uD604\uD569\uB2C8\uB2E4."
title: "\uC624\uB958 \uCC98\uB9AC"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
Dart에서 오류 처리는 프로그램 실행 중 발생하는 예외를 예상하고 관리하여 신뢰성과 사용성을 향상시키는 것에 관한 것입니다. 프로그래머들은 충돌을 방지하고 사용자에게 의미 있는 피드백을 제공하여 더 매끄럽고 안전한 애플리케이션 경험을 보장하기 위해 오류 처리를 구현합니다.

## 방법:
Dart는 *컴파일 시간* 오류와 *실행 시간* 오류라는 두 가지 유형의 오류를 지원합니다. 컴파일 시간 오류는 코드가 실행되기 전에 Dart 분석기에 의해 감지되며, 실행 시간 오류 또는 예외는 실행 중에 발생합니다. 다음은 Dart에서 예외를 처리하는 방법입니다:

### Try-Catch
`try-catch`를 사용하여 예외를 캡처하고 애플리케이션이 충돌하는 것을 방지하세요:

```dart
try {
  var result = 100 ~/ 0; // 0으로 나누기 시도, 예외 발생
} catch (e) {
  print('예외를 캐치했습니다: $e'); // 예외 처리
}
```
샘플 출력: `예외를 캐치했습니다: IntegerDivisionByZeroException`

### 특정 예외
특정 예외를 처리하려면 `catch` 뒤에 예외를 명시하세요:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('0으로 나눌 수 없습니다.'); // 0으로 나누는 예외를 구체적으로 처리
}
```
샘플 출력: `0으로 나눌 수 없습니다.`

### 스택 추적
디버깅을 위한 스택 추적을 얻으려면 catch 블록에서 두 번째 파라미터를 사용하세요:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('예외: $e');
  print('스택 추적: $s'); // 디버깅을 위한 스택 추적 출력
}
```

### Finally
`finally`를 사용하여 try/catch 이후에 예외가 발생했는지 여부에 상관없이 코드를 실행하세요:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('예외를 캐치했습니다: $e');
} finally {
  print('이 코드는 항상 실행됩니다.'); // 정리 코드 또는 마지막 단계
}
```
샘플 출력:
```
예외를 캐치했습니다: IntegerDivisionByZeroException
이 코드는 항상 실행됩니다.
```

### 제3자 라이브러리
Dart의 핵심 라이브러리가 오류 처리에 강력하긴 하지만, `Either`와 `Option`과 같은 개념을 도입하는 함수형 프로그래밍을 위해 `dartz`와 같은 제3자 패키지를 사용할 수도 있습니다. 다음은 `dartz`를 사용하여 오류 처리하는 예시입니다:

1. 의존성 아래 `pubspec.yaml` 파일에 `dartz`를 추가하세요:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Dart 코드에서 오류를 우아하게 처리하기 위해 `Either`를 사용하세요:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('0으로 나눌 수 없습니다.');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('오류: $left'), 
    (right) => print('결과: $right')
  );
}
```
샘플 출력: `오류: 0으로 나눌 수 없습니다.`

`Left` 부분은 일반적으로 오류를 나타내며, `Right` 부분은 성공을 나타냅니다. 이 패턴은 오류 처리를 더 기능적인 방식으로 다룰 수 있게 해 줄뿐만 아니라 오류 관리를 통해 명확성과 제어를 제공합니다.
