---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:40.781937-07:00
description: "Google Apps Script(GAS)\uC5D0\uC11C \uB514\uBC84\uAE45\uC740 Google\
  \ Apps\uB97C \uC790\uB3D9\uD654\uD558\uAC70\uB098 \uC6F9 \uC560\uD50C\uB9AC\uCF00\
  \uC774\uC158\uC744 \uAD6C\uCD95\uD558\uAE30 \uC704\uD574 \uC758\uB3C4\uB41C \uC2A4\
  \uD06C\uB9BD\uD2B8\uC5D0\uC11C \uC624\uB958\uB97C \uC2DD\uBCC4\uD558\uACE0 \uC81C\
  \uAC70\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uCF54\uB4DC\uAC00 \uC608\uC0C1\uB300\uB85C \uC2E4\uD589\uB418\uB3C4\
  \uB85D \uD558\uC5EC \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uC2E0\uB8B0\uC131\
  \uACFC \uC131\uB2A5\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uAE30 \uC704\uD574\u2026"
lastmod: '2024-02-25T18:49:51.577508-07:00'
model: gpt-4-0125-preview
summary: "Google Apps Script(GAS)\uC5D0\uC11C \uB514\uBC84\uAE45\uC740 Google Apps\uB97C\
  \ \uC790\uB3D9\uD654\uD558\uAC70\uB098 \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \uC744 \uAD6C\uCD95\uD558\uAE30 \uC704\uD574 \uC758\uB3C4\uB41C \uC2A4\uD06C\uB9BD\
  \uD2B8\uC5D0\uC11C \uC624\uB958\uB97C \uC2DD\uBCC4\uD558\uACE0 \uC81C\uAC70\uD558\
  \uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uCF54\uB4DC\uAC00 \uC608\uC0C1\uB300\uB85C \uC2E4\uD589\uB418\uB3C4\uB85D \uD558\
  \uC5EC \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uC2E0\uB8B0\uC131\uACFC \uC131\
  \uB2A5\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uAE30 \uC704\uD574\u2026"
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Google Apps Script(GAS)에서 디버깅은 Google Apps를 자동화하거나 웹 애플리케이션을 구축하기 위해 의도된 스크립트에서 오류를 식별하고 제거하는 과정입니다. 프로그래머들은 코드가 예상대로 실행되도록 하여 애플리케이션의 신뢰성과 성능을 향상시키기 위해 디버깅을 합니다.

## 방법:

Google Apps Script는 스크립트를 문제 해결하기 위해 Apps Script 에디터 내에 내장된 디버거를 제공합니다. 디버거를 시작하고 사용하는 방법은 다음과 같습니다:

1. **Apps Script 에디터에서 스크립트를 엽니다.**
2. **디버그할 함수를 선택합니다.** 상단의 드롭다운 메뉴에서 디버그하고 싶은 함수를 선택합니다.
3. **중단점을 설정합니다.** 실행을 중지하고 싶은 줄 번호의 왼쪽에 있는 회색 영역(거터)를 클릭하면, 붉은 점이 나타나 중단점을 나타냅니다.
4. **디버깅을 시작합니다.** 버그 아이콘을 클릭하거나 `디버그` > `디버깅 시작`을 선택합니다. 실행이 시작되고 첫 번째 중단점에서 일시 중단됩니다.

간단한 스크립트를 고려해 보세요:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // 15를 기록하려고 함
}
```

`Logger.log(sum)`이 예상한 결과를 표시하지 않는 이유가 확실하지 않다면, `var sum = a + b;` 라인에서 중단점을 설정하고 스크립트를 한 줄씩 진행하며 변수 값들을 검사할 수 있습니다.

**Logger에서의 샘플 출력:**

```plain
15
```

디버깅하는 동안, Apps Script 에디터는 다음을 허용합니다:

- 코드를 **단계별로 진행**하기 위해 스텝 오버, 스텝 인투, 스텝 아웃 버튼을 사용합니다.
- 실시간으로 값이 변하는 **표현식과 변수를 관찰**합니다.
- 함수 호출을 추적하기 위해 **호출 스택을 검사**합니다.

## 심화 학습

Google Apps Script에서의 디버깅은 다른 프로그래밍 환경과 마찬가지로 오류 없는 애플리케이션을 만들기 위해 필수적입니다. GAS의 개발 초기에 도입된 내장 디버거는 코드를 증분적으로 검사하고 수정할 수 있는 기본적인 능력을 제공합니다. Visual Studio Code나 IntelliJ와 같은 더 성숙한 환경에서 찾을 수 있는 기본 디버깅 기능과 유사하긴 하지만, 복잡한 디버깅 시나리오에는 부족할 수 있습니다. 예를 들어, 비동기 콜백을 검사하거나 무거운 스크립트 실행을 관리하는 능력이 제한적일 수 있습니다.

복잡한 디버깅이 필요한 경우, 개발자들은 `Logger.log()`를 사용하는 광범위한 로깅이나 실제 상황에서의 동작을 검사하기 위해 웹 앱으로 배포하는 등의 대체 방법을 선택할 수 있습니다. 그러나 Apps Script 에디터 내에 통합된 GAS의 디버거의 단순성은 스크립트 동작을 이해하고 문제를 해결하기 위한 첫 번째 단계로서 매우 소중합니다. 특히 Google이 Apps Script에 지속적인 업데이트와 개선을 가함에 따라, 디버깅 경험은 점차 향상되어 시간이 지남에 따라 더 정교한 도구와 옵션을 제공하고 있습니다. 이러한 발전은 Google이 개발자들에게 더 강력하고 접근하기 쉬운 플랫폼을 만들기 위한 지속적인 약속을 반영합니다.
