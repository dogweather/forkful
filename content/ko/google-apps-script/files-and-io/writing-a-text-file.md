---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:35.454089-07:00
description: "Google Apps Script\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744\
  \ \uC791\uC131\uD558\uBA74 \uAC1C\uBC1C\uC790\uB4E4\uC774 \uB370\uC774\uD130\uB97C\
  \ \uC9C0\uC18D\uC801\uC73C\uB85C \uC800\uC7A5\uD558\uC5EC \uD5A5\uD6C4 \uC0AC\uC6A9\
  \uC774\uB098 \uBD84\uC11D\uC744 \uC704\uD574 \uC811\uADFC\uD560 \uC218 \uC788\uAC8C\
  \ \uD574\uC90D\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uB85C\uAE45, \uC124\uC815\
  \ \uC800\uC7A5, \uC815\uBCF4\uB97C \uAC04\uB2E8\uD558\uACE0 \uC77D\uAE30 \uC26C\uC6B4\
  \ \uD615\uC2DD\uC73C\uB85C \uB0B4\uBCF4\uB0B4\uAE30 \uB4F1\uC5D0 \uC788\uC5B4 \uC77C\
  \uBC18\uC801\uC778 \uAD00\uD589\uC785\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:13.503342
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\
  \uC131\uD558\uBA74 \uAC1C\uBC1C\uC790\uB4E4\uC774 \uB370\uC774\uD130\uB97C \uC9C0\
  \uC18D\uC801\uC73C\uB85C \uC800\uC7A5\uD558\uC5EC \uD5A5\uD6C4 \uC0AC\uC6A9\uC774\
  \uB098 \uBD84\uC11D\uC744 \uC704\uD574 \uC811\uADFC\uD560 \uC218 \uC788\uAC8C \uD574\
  \uC90D\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uB85C\uAE45, \uC124\uC815 \uC800\uC7A5\
  , \uC815\uBCF4\uB97C \uAC04\uB2E8\uD558\uACE0 \uC77D\uAE30 \uC26C\uC6B4 \uD615\uC2DD\
  \uC73C\uB85C \uB0B4\uBCF4\uB0B4\uAE30 \uB4F1\uC5D0 \uC788\uC5B4 \uC77C\uBC18\uC801\
  \uC778 \uAD00\uD589\uC785\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC791\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Google Apps Script에서 텍스트 파일을 작성하면 개발자들이 데이터를 지속적으로 저장하여 향후 사용이나 분석을 위해 접근할 수 있게 해줍니다. 이 작업은 로깅, 설정 저장, 정보를 간단하고 읽기 쉬운 형식으로 내보내기 등에 있어 일반적인 관행입니다.

## 방법:

Google Apps Script에서 텍스트 파일을 생성하고 작성하는 것은 Google DriveApp 서비스를 통해 수행될 수 있습니다. 아래에 코드 예시와 함께 시작하는 데 도움이 될 단계별 가이드가 있습니다:

**1단계: 새 텍스트 파일 생성**

```javascript
// Google Drive의 루트에 새 텍스트 파일을 생성
var file = DriveApp.createFile('Example.txt', 'Hello, world!');
```

이 코드 조각은 "Hello, world!"라는 내용을 포함한 "Example.txt"라는 이름의 텍스트 파일을 생성합니다.

**2단계: 기존 텍스트 파일 열기 및 작성**

기존 파일을 열고 그 안에 작성해야 하는 경우, `getFileById(id)` 메소드를 사용하여 파일을 검색한 다음 그 내용을 조작할 수 있습니다.

```javascript
// 파일 ID로 파일을 가져오고 새로운 내용을 추가
var fileId = 'YOUR_FILE_ID_HERE'; // YOUR_FILE_ID_HERE를 실제 파일 ID로 대체하세요
var file = DriveApp.getFileById(fileId);
file.setContent(file.getBlob().getDataAsString() + '\nNew content added.');
```

이 코드는 고유 ID를 사용하여 기존 파일을 검색한 다음, 이전에 있던 내용 뒤에 "New content added."를 추가합니다.

**샘플 출력**

위 코드 조각을 실행해도 명시적인 출력은 표시되지 않지만, 파일이 위치한 Google Drive로 이동하면, 첫 번째 코드 조각에 대해서는 "Example.txt"를 확인할 수 있습니다. 두 번째 조각의 경우, ID로 지정된 파일을 열면 원래의 내용에 "New content added."라는 새로운 줄이 추가된 것을 볼 수 있습니다.

## 상세 분석

Google Apps Script에서 텍스트 파일을 작성하는 것은 DriveApp 서비스를 활용하며, 본질적으로 파일 저장 및 관리를 위한 Google Drive의 기능을 활용합니다. 이 접근 방법은 Google Apps Script가 처음 만들어졌을 때로 거슬러 올라가는데, 이는 Google의 생산성 도구 모음(드라이브 포함)에서 작업을 쉽게 자동화하도록 설계되었습니다.

Google Apps Script를 통한 파일 직접 조작은 Google Workspace와 밀접하게 통합되어 있고 직관적이지만, 다른 배경(예: Python, Node.js)을 가진 개발자들에게는 로컬 파일 시스템이나 AWS S3 같은 다른 클라우드 저장 서비스와 작업하는 것과 다를 수 있습니다. 이러한 플랫폼들은 종종 더 복잡한 파일 조작 기능을 제공하지만 인증 및 권한 설정을 위해 추가적인 설정이 필요합니다.

단순 텍스트 파일을 넘어서 더 복잡한 파일 관리나 처리 기능(예: 이진 데이터 처리나 광범위한 파일 시스템 작업)이 필요한 시나리오의 경우, 개발자는 Google Apps Script와 함께 Google Cloud Platform 서비스(예: Cloud Storage)를 사용하는 것을 고려할 수 있습니다. 이러한 대안은 더 강력하지만, 프로젝트의 범위에 따라 배워야 할 내용이 더 많고 비용도 더 높을 수 있습니다.

결론적으로, Google Apps Script는 Google Drive 내에서 파일을 관리하고, 텍스트 파일을 작성하는 등 효율적이고 접근하기 쉬운 방법을 제공하지만, 그 한계를 이해하고 더 복잡한 요구 사항을 충족하기 위해 필요에 따라 다른 Google 기술을 탐색하는 것이 중요합니다.
