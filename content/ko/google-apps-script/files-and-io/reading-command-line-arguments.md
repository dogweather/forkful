---
title:                "명령 줄 인수 읽기"
aliases: - /ko/google-apps-script/reading-command-line-arguments.md
date:                  2024-02-01T22:02:06.079514-07:00
model:                 gpt-4-0125-preview
simple_title:         "명령 줄 인수 읽기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/google-apps-script/reading-command-line-arguments.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Google Apps Script에서 명령줄 인수를 읽는 것은 Python이나 Node.js와 같은 프로그래밍 언어에서의 전통적인 명령줄 인터페이스와 달리, Google Apps Script가 본질적으로 명령줄 실행이나 인수 파싱을 지원하지 않기 때문에 약간의 잘못된 명명일 수 있습니다. 대신, 코더들은 종종 웹 앱이나 자동화된 작업을 실행할 때 사용자 입력이나 사전 정의된 매개변수에 기반하여 스크립트 기능과 동적으로 상호작용하게 하는 사용자 정의 함수와 URL 매개변수를 통해 이 과정을 모방합니다.

## 방법:

Google Apps Script에서 명령줄 인수를 읽는 과정을 흉내내려면, 특히 웹 앱의 경우, 쿼리 문자열 매개변수를 사용할 수 있습니다. 사용자가 웹 앱 URL에 접근할 때, `?name=John&age=30`과 같은 인수를 추가하고 이를 Apps Script 코드 내에서 분석할 수 있습니다. 이것이 설정 방법입니다:

```javascript
function doGet(e) {
  var params = e.parameter; // 쿼리 문자열 매개변수를 검색합니다
  var name = params['name']; // 'name' 매개변수를 가져옵니다
  var age = params['age']; // 'age' 매개변수를 가져옵니다

  // 샘플 출력:
  var output = "Name: " + name + ", Age: " + age;
  return HtmlService.createHtmlOutput(output);
}

// 예시 URL: https://script.google.com/macros/s/your_script_id/exec?name=John&age=30
```

지정된 매개변수로 URL에 접근하면 스크립트는 다음과 같은 것을 출력합니다:

```
Name: John, Age: 30
```

이 접근법은 웹 앱에서 개인화된 상호작용을 생성하거나 스크립트 실행을 프로그래밍 방식으로 제어하는 데 유용합니다.

## 심층 분석

전통적인 프로그래밍 언어의 컨텍스트에서 이해되는 명령줄 인수는 사용자 입력이나 자동화된 프로세스를 기반으로 유연하고 동적인 코드 실행을 가능하게 하는 스크립트 및 애플리케이션의 기능을 제공합니다. Google Apps Script는 Google Workspace 생태계 내에서 경량 애플리케이션 개발을 위한 클라우드 기반 스크립팅 언어로, 본래 명령줄 인터페이스를 통해 작동하지 않습니다. 대신, 그 실행은 주로 이벤트 기반으로, 또는 Apps Script 및 Google Workspace UI를 통해 수동으로 트리거되거나, URL 매개변수를 사이도 명령줄 인수로 구문 분석할 수 있는 웹 앱을 통해 이루어집니다.

이러한 아키텍처 차이를 고려할 때, CLI 중심 언어의 배경을 가진 프로그래머들은 Google Apps Script에서 작업을 자동화하거나 애플리케이션을 개발할 때 접근 방식을 조정해야 할 수 있습니다. 전통적인 명령줄 인수 파싱 대신, Google Apps Script의 웹 앱 기능이나 심지어 Google 시트 사용자 정의 함수를 활용하여 대화형 데이터 처리를 제공하는 것은 비슷한 목적을 섬깁니다. 처음에는 제한으로 보일 수 있지만, 이는 더 사용자 친화적인 인터페이스와 접근 가능한 웹 애플리케이션의 개발을 장려하며, Google Apps Script가 Google Workspace 애플리케이션의 원활한 통합 및 확장에 중점을 두는 것과 일치합니다.

CLI 동작의 더욱 밀접한 모방이 중요한 시나리오에서 (예: 동적 매개변수를 사용한 작업의 자동화) 개발자는 Google Apps Script 웹 앱을 호출하고, URL을 통해 매개변수를 전달하는 방법으로 외부 플랫폼을 활용할 수 있습니다. 그러나, 토종 Google Apps Script 프로젝트의 경우, 플랫폼의 이벤트 중심 및 UI 중심 모델을 채택하는 것이 종종 더 직관적이고 유지 보수가 용이한 솔루션으로 이어집니다.
