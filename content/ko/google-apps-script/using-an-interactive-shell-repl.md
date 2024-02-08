---
title:                "인터랙티브 셸(REPL) 사용하기"
aliases:
- ko/google-apps-script/using-an-interactive-shell-repl.md
date:                  2024-02-01T22:04:10.708649-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸(REPL) 사용하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/google-apps-script/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

인터랙티브 셸, 또는 읽기-평가-출력 루프(REPL)는 단일 사용자 입력(표현식)을 받아 평가하고 사용자에게 결과를 반환하는 간단한 인터랙티브 프로그래밍 환경입니다. 프로그래머들은 REPL을 사용하여 빠른 프로토타이핑, 디버깅 및 프로그래밍 언어의 문법과 동작을 인터랙티브하게 학습합니다.

## 방법:

Google Apps Script는 Google 제품 간의 작업을 자동화하기 위한 클라우드 기반 스크립팅 언어로, Python이나 JavaScript의 Node.js와 같은 언어에서 볼 수 있는 내장 REPL 도구가 없습니다. 그러나 Apps Script 편집기의 로깅 및 디버깅 기능을 사용하거나 외부 환경을 설정함으로써 비슷한 경험을 시뮬레이션할 수 있습니다. 여기서는 Apps Script 편집기 내에서 임시 REPL을 만드는 방법에 초점을 맞춥니다.

1. **임시 REPL 함수 만들기**:

```javascript
function myREPL() {
  var input = Logger.log('표현식을 입력하세요: ');
  try {
    var result = eval(input);
    Logger.log('결과: ' + result);
  } catch(e) {
    Logger.log('오류: ' + e.message);
  }
}
```

Apps Script 환경에서는 전통적인 REPL과 같은 방식으로 직접 사용자 입력을 받는 것이 불가능하기 때문에, `input` 변수를 수동으로 수정하고 `myREPL()`을 실행하여 표현식을 테스트할 수 있습니다.

2. **샘플 코드 실행**:

`2+2`를 평가하고 싶다고 가정해 봅시다. `myREPL` 함수를 다음과 같이 수정합니다:

```javascript
function myREPL() {
  var input = '2+2'; // 여기에 수동으로 표현식을 입력하세요
  // 나머지는 동일하게 유지...
}
```

`myREPL()`을 실행한 후, 로그(View > 로그)를 확인하면 다음과 같은 출력을 확인할 수 있습니다:

```
[20-xx-xxxx xx:xx:xx:xxx] 표현식을 입력하세요:
[20-xx-xxxx xx:xx:xx:xxx] 결과: 4
```

3. **Logger를 사용한 디버깅**:

보다 복잡한 디버깅을 위해, 코드 내부에 `Logger.log(변수);`를 삽입하여 변수 상태를 출력함으로써 스크립트의 흐름과 중간 상태를 이해하는 데 도움을 줍니다.

## 깊이 있는 탐구

REPL의 개념은 1960년대의 시간 공유 시스템에서 비롯되어 상호작용 세션을 허용하는 컴퓨팅 역사에 깊이 뿌리박고 있습니다. Lisp과 같은 언어는 이러한 환경에서 번창했으며, REPL은 그들의 반복적 개발 과정에 있어 필수적이었습니다. 반면, 훨씬 늦게 나타난 Google Apps Script는 주로 웹에 초점을 맞추며, Google의 제품군 내에서 작업을 자동화하는 데 중점을 둔 설계입니다.

Google Apps Script는 클라우드 기반의 성격과 웹 앱 배포 초점으로 인해 기본적으로 실시간, 인터랙티브한 코딩 세션을 지원하지 않습니다. 해당 실행 모델은 즉각적인 피드백 루프를 제공하는 REPL이 아닌, 웹 이벤트, 시간 기반 트리거 또는 환경 내에서의 수동 호출에 의해 트리거되는 함수들을 중심으로 회전합니다.

Apps Script 편집기 내의 임시 REPL 및 디버거는 일정 수준의 상호작용을 제공하지만, 많은 프로그래밍 언어에서 찾아볼 수 있는 전통적인 REPL의 즉각적인 피드백과 효율성을 완전히 복제하지는 못합니다. Google 기술과 함께 더 진정한 REPL 경험을 찾는 개발자들은 Google의 API와 함께하는 외부 JavaScript 환경이나 Node.js를 탐색할 수 있습니다. 이러한 환경은 더욱 반응적이고 상호작용적인 코딩 세션을 제공할 수 있지만, 더 많은 설정이 필요하고 직접적인 Apps Script 환경을 벗어날 수도 있습니다.
