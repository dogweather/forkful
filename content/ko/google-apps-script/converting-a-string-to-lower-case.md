---
title:                "문자열을 소문자로 변환하기"
aliases:
- ko/google-apps-script/converting-a-string-to-lower-case.md
date:                  2024-02-01T21:51:20.479369-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열을 소문자로 변환하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/google-apps-script/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Google Apps Script에서 문자열을 소문자로 변환하는 것은 Google 제품에서 작업을 자동화하기 위한 클라우드 기반 스크립트 언어에서 표준 텍스트 데이터를 목표로 하는 기본적인 작업입니다. 프로그래머들은 사용자 입력, 데이터 처리, 또는 문자열을 비교할 때 일관성을 확보하기 위해 이 작업을 자주 수행하며, 대소문자 구분 문제를 없애줍니다.

## 방법:

Google Apps Script에서 문자열을 소문자로 변환하는 것은 스크립팅 환경 내에서 사용할 수 있는 내장 JavaScript 메소드 덕분에 간단합니다. 주로 사용하는 메소드는 `toLowerCase()`입니다. 이를 구현하는 방법은 다음과 같습니다:

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // 출력: hello, world!
}
```

이 간단한 함수는 원래 문자열을 가져와 `toLowerCase()` 메소드를 적용하고 결과를 로깅하는 방법을 보여줍니다. 이는 대소문자를 구분하지 않아야 하는 입력을 처리할 때 특히 유용합니다. 예를 들어, 사용자가 다양한 대소문자로 입력할 수 있는 이메일 주소를 비교하는 경우입니다.

또한 배열 데이터를 다루는 상황에서는 각 요소를 소문자로 변환하기 위해 배열을 매핑하는 방법을 사용할 수 있습니다:

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // 출력: [alice, bob, charlie]
}
```

이 예는 `toLowerCase()`가 문자열 데이터의 복수를 처리할 때의 다재다능함을 강조하며, 데이터 세트 전반에 걸쳐 일관성을 보장합니다.

## 심층 분석

JavaScript에서 상속받은 `toLowerCase()` 메소드는 Google Apps Script 내에서 사용되며, 자바스크립트 초기 버전부터 문자열 조작에서 중요한 부분을 차지해 왔습니다. 이 메소드의 주된 목적은 동적이고 사용자 상호 작용 웹 애플리케이션이 등장하면서 생겨난 텍스트 데이터의 대소문자 구분을 처리하는 데 도움을 주는 것입니다. 그 간단함에도 불구하고, 이 메커니즘은 대소문자 구분으로 인해 도입되는 복잡성을 줄이면서 데이터 검증, 정렬, 검색 알고리즘에서 중요한 역할을 합니다.

성능 측면에서, 변환 프로세스는 현대의 JavaScript 엔진에서 매우 최적화되었습니다; 그러나 대규모 데이터 작업 내에서 불필요한 처리 오버헤드를 피하기 위해 그 적용을 신중히 해야 합니다.

특히 복잡한 패턴을 다루거나 로케일별 변환을 필요로 하는 경우 고려해볼 수 있는 대안은 `toLocaleLowerCase()` 메소드입니다. 이 변형은 문자를 소문자로 변환하는 데 로케일별 규칙을 고려합니다. 이는 다중 언어를 지원하는 애플리케이션에 필수적일 수 있습니다:

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // 출력: märz
```

추가적인 복잡성에도 불구하고, `toLocaleLowerCase()`는 사용자의 로케일의 언어 규범을 존중하는 변환을 보장함으로써 국제 애플리케이션을 위한 강력한 도구입니다. 사용하는 메소드가 무엇이든, Google Apps Script에서 문자열을 소문자로 변환하는 것은 사용자 입력과 표준화된 데이터 처리 사이의 간극을 메우는 필수적인 부분입니다.
