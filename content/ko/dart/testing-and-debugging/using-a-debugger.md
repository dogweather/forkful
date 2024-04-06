---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:44.105563-07:00
description: "\uBC29\uBC95: **1. \uC911\uB2E8\uC810 \uC124\uC815:** \uC911\uB2E8\uC810\
  \uC744 \uC124\uC815\uD558\uB824\uBA74, \uC2E4\uD589\uC744 \uC77C\uC2DC \uC911\uC9C0\
  \uD558\uACE0\uC790 \uD558\uB294 \uCF54\uB4DC \uB77C\uC778\uC758 \uC67C\uCABD \uC5EC\
  \uBC31\uC744 IDE(\uC608: Visual Studio Code \uB610\uB294 Android Studio)\uC5D0\uC11C\
  \ \uD074\uB9AD\uD558\uBA74 \uB429\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.795994-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

## 방법:


### 기본 디버깅:
**1. 중단점 설정:**

중단점을 설정하려면, 실행을 일시 중지하고자 하는 코드 라인의 왼쪽 여백을 IDE(예: Visual Studio Code 또는 Android Studio)에서 클릭하면 됩니다.

```dart
void main() {
  var message = 'Hello, Debugging';
  print(message); // 여기에 중단점을 설정하세요
}
```

**2. 디버깅 시작:**

IDE에서 디버그 아이콘을 클릭하거나 디버그 버튼을 눌러 디버깅 세션을 시작합니다. 실행은 중단점에서 일시 중지됩니다.

**3. 변수 검사:**

실행이 일시 중지되면, 변수 위에 마우스를 올려 그 현재 값을 확인합니다.

**4. 코드 단계별 진행:**

IDE에서 한 줄 또는 하나의 함수씩 코드를 넘어가며 진행할 수 있는 스텝 오버, 스텝 인투, 스텝 아웃 명령을 사용합니다.

### Observatory를 사용한 고급 디버깅:
Dart는 Dart 애플리케이션의 디버깅 및 프로파일링을 위한 Observatory라는 도구를 포함하고 있습니다. Dart VM에서 실행 중인 애플리케이션에 특히 유용합니다.

**Observatory 접근:**

`--observe` 플래그와 함께 Dart 애플리케이션을 실행합니다.

```bash
dart --observe your_program.dart
```

이 명령은 콘솔에 URL을 출력하는데, 이를 웹 브라우저에서 열어 Observatory 디버거에 접속할 수 있습니다.

### 인기 있는 서드파티 라이브러리 사용하기:
Flutter 애플리케이션의 디버깅에는 `flutter_devtools` 패키지가 Dart VM과 Flutter 모두와 통합되는 성능 및 디버깅 도구 모음을 제공합니다.

**설치:**

먼저, `dev_dependencies` 아래 `pubspec.yaml` 파일에 `devtools`를 추가합니다:

```yaml
dev_dependencies:
  devtools: any
```

**DevTools 실행:**

터미널에서 이 명령을 실행합니다:

```bash
flutter pub global run devtools
```

그다음, Flutter 애플리케이션을 디버그 모드로 시작합니다. DevTools는 위젯 트리 분석을 위한 Flutter 인스펙터와 네트워크 활동 모니터링을 위한 네트워크 프로파일러와 같은 기능을 제공합니다.

### 샘플 출력:
중단점에 도달하면, IDE는 다음과 같은 변수 값과 스택 트레이스를 표시할 수 있습니다:

```
message: 'Hello, Debugging'
```

Dart에서 디버깅 도구와 기술을 효과적으로 활용함으로써, 개발자는 문제를 더 빠르게 식별하고 해결할 수 있어 개발 과정이 원활해지고 애플리케이션이 더 견고해집니다.
