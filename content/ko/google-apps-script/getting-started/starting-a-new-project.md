---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:47.649985-07:00
description: "\uBC29\uBC95: Google Apps Script\uC5D0\uC11C \uC0C8 \uD504\uB85C\uC81D\
  \uD2B8\uB97C \uC2DC\uC791\uD558\uB824\uBA74 \uBA87 \uAC00\uC9C0 \uC9C4\uC785\uC810\
  \uC774 \uC788\uC9C0\uB9CC, \uAC00\uC7A5 \uC9C1\uC811\uC801\uC778 \uBC29\uBC95\uC5D0\
  \ \uC9D1\uC911\uD574\uBD05\uC2DC\uB2E4: Google \uB4DC\uB77C\uC774\uBE0C\uC5D0\uC11C\
  \ \uC2A4\uD06C\uB9BD\uD2B8 \uC0DD\uC131\uD558\uAE30. 1. **Google \uB4DC\uB77C\uC774\
  \uBE0C\uC5D0\uC11C \uD504\uB85C\uC81D\uD2B8 \uC0DD\uC131\uD558\uAE30** - Google\
  \ \uB4DC\uB77C\uC774\uBE0C\uB85C\u2026"
lastmod: '2024-03-13T22:44:54.531181-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uC0C8 \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\
  \uC791\uD558\uB824\uBA74 \uBA87 \uAC00\uC9C0 \uC9C4\uC785\uC810\uC774 \uC788\uC9C0\
  \uB9CC, \uAC00\uC7A5 \uC9C1\uC811\uC801\uC778 \uBC29\uBC95\uC5D0 \uC9D1\uC911\uD574\
  \uBD05\uC2DC\uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## 방법:
Google Apps Script에서 새 프로젝트를 시작하려면 몇 가지 진입점이 있지만, 가장 직접적인 방법에 집중해봅시다: Google 드라이브에서 스크립트 생성하기.

1. **Google 드라이브에서 프로젝트 생성하기**
   - Google 드라이브로 이동하세요(drive.google.com).
   - "+ 새로 만들기" > "기타" > "Google Apps Script"를 클릭하세요.
   - 새 스크립트 프로젝트가 편집기에서 열립니다. 기본적으로 `Code.gs` 파일이 샘플 `myFunction`을 포함하고 있습니다.

2. **프로젝트 설정하기**
   - 프로젝트를 명확하게 하기 위해 이름을 바꾸세요. 왼쪽 상단의 "제목 없는 프로젝트"를 클릭하고 의미 있는 이름을 부여하세요.
   - `Code.gs` 파일에 간단한 함수를 작성하여 느낌을 얻어보세요:

```javascript
function helloWorld() {
  Logger.log('안녕, 세상아!');
}
```

   - 재생 버튼(▶) 옆의 드롭다운에서 `helloWorld` 함수를 선택하고 클릭하여 실행하세요. 이렇게 하면 함수가 실행됩니다.

3. **로그 보기**
   - `Logger.log`의 출력을 보려면 "보기" > "로그"로 가거나 `Ctrl + Enter`를 누릅니다. 로그에서 "안녕, 세상아!"를 볼 수 있습니다.

축하합니다, Google Apps Script에서 새 프로젝트를 성공적으로 시작하고 간단한 함수를 실행했습니다!

## 심층 탐구
2009년경 Google Apps Script의 시작은 개발자 및 비개발자 모두에게 Google 서비스의 광범위한 배열을 자동화하고, 확장하고, 구축할 수 있는 강력하면서도 접근하기 쉬운 플랫폼을 제공했습니다. 전통적인 프로그래밍 환경과 달리, GAS는 외부 서버나 설정 없이 바로 Google 생태계 내에서의 단순함과 통합을 제공하는 독특한 조합을 제공합니다. 이 서버리스 실행 모델은 프로젝트 배포 및 관리를 크게 단순화합니다.

역사적으로, GAS는 실행 환경과 언어 버전으로 인해 종종 현재 JavaScript 표준에 뒤처졌습니다. 그러나 최근에는 현대 JavaScript 문법(ECMAScript 2015+)이 GAS에 도입되어 현대적인 개발 관행에 익숙한 개발자들에게 더 적합하게 만들었습니다.

GAS는 Google 서비스와 상호작용하기에 독특하게 위치해 있지만, 더 집중적이거나 특정한 필요를 위한 대안적 접근 방법이 있습니다. 예를 들어, Google Cloud Functions와 Google Cloud Platform(GCP)은 복잡한 워크플로우를 처리하고, 대규모 데이터 세트를 처리하며, 외부 API와 통합하기 위한 더 강력하고 확장 가능한 솔루션을 제공합니다. 이러한 플랫폼들은 다양한 언어(Python, Go, Node.js 등)로 프로그래밍할 수 있으며 더 큰 계산 자원을 제공합니다.

그럼에도 불구하고 Google Apps와 밀접하게 연결된 작업, 자동화, 그리고 이 생태계 내에서의 빠른 개발을 위해 Google Apps Script는 사용 편의성과 통합 깊이 측면에서 비교할 수 없는 도구로 남아 있습니다. Google 드라이브에서 바로 접근할 수 있고 Google 서비스와의 원활한 연결은 시트, 문서, 폼 및 기타 Google 애플리케이션의 기능을 확장하고자 하는 다양한 프로젝트에 실용적인 선택을 제공합니다.
