---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:01.240991-07:00
description: "\uBC29\uBC95: Dart\uB294 *\uCEF4\uD30C\uC77C \uC2DC\uAC04* \uC624\uB958\
  \uC640 *\uC2E4\uD589 \uC2DC\uAC04* \uC624\uB958\uB77C\uB294 \uB450 \uAC00\uC9C0\
  \ \uC720\uD615\uC758 \uC624\uB958\uB97C \uC9C0\uC6D0\uD569\uB2C8\uB2E4. \uCEF4\uD30C\
  \uC77C \uC2DC\uAC04 \uC624\uB958\uB294 \uCF54\uB4DC\uAC00 \uC2E4\uD589\uB418\uAE30\
  \ \uC804\uC5D0 Dart \uBD84\uC11D\uAE30\uC5D0 \uC758\uD574 \uAC10\uC9C0\uB418\uBA70\
  , \uC2E4\uD589 \uC2DC\uAC04 \uC624\uB958 \uB610\uB294 \uC608\uC678\uB294 \uC2E4\uD589\
  \ \uC911\uC5D0 \uBC1C\uC0DD\uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740 Dart\uC5D0\uC11C\
  \ \uC608\uC678\uB97C \uCC98\uB9AC\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4:\u2026"
lastmod: '2024-03-13T22:44:54.800921-06:00'
model: gpt-4-0125-preview
summary: "Dart\uB294 *\uCEF4\uD30C\uC77C \uC2DC\uAC04* \uC624\uB958\uC640 *\uC2E4\uD589\
  \ \uC2DC\uAC04* \uC624\uB958\uB77C\uB294 \uB450 \uAC00\uC9C0 \uC720\uD615\uC758\
  \ \uC624\uB958\uB97C \uC9C0\uC6D0\uD569\uB2C8\uB2E4."
title: "\uC624\uB958 \uCC98\uB9AC"
weight: 16
---

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
