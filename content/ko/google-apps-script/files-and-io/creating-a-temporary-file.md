---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:17.700089-07:00
description: "\uBC29\uBC95: Google Apps Script\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\
  \uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC740 Google Drive\uC5D0\uC11C \uD30C\uC77C\
  \uC744 \uC0DD\uC131, \uC77D\uAE30 \uBC0F \uC0AD\uC81C\uD558\uB294 \uAC04\uB2E8\uD55C\
  \ \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\uB294 DriveApp \uC11C\uBE44\uC2A4\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uB2EC\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\
  \uAE30\uC5D0 \uB370\uC774\uD130\uB97C \uC791\uC131\uD55C \uD6C4 \uC0AC\uC6A9 \uD6C4\
  \ \uC0AD\uC81C\uD558\uB294 \uC784\uC2DC \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744\u2026"
lastmod: '2024-03-13T22:44:54.563449-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\uC744 \uC0DD\uC131\
  \uD558\uB294 \uAC83\uC740 Google Drive\uC5D0\uC11C \uD30C\uC77C\uC744 \uC0DD\uC131\
  , \uC77D\uAE30 \uBC0F \uC0AD\uC81C\uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744\
  \ \uC81C\uACF5\uD558\uB294 DriveApp \uC11C\uBE44\uC2A4\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uB2EC\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

## 방법:
Google Apps Script에서 임시 파일을 생성하는 것은 Google Drive에서 파일을 생성, 읽기 및 삭제하는 간단한 방법을 제공하는 DriveApp 서비스를 사용하여 달성할 수 있습니다. 여기에 데이터를 작성한 후 사용 후 삭제하는 임시 텍스트 파일을 생성하는 방법이 있습니다:

```javascript
function createTemporaryFile() {
  // "tempFile.txt"라는 이름의 임시 파일 생성
  var tempFile = DriveApp.createFile('tempFile.txt', 'Temporary content', MimeType.PLAIN_TEXT);
  
  // 접근 또는 디버깅을 위한 파일 URL 기록
  Logger.log('Temporary file created: ' + tempFile.getUrl());
  
  // 예제 작업: 파일 내용 읽기
  var content = tempFile.getBlob().getDataAsString();
  Logger.log('tempFile의 내용: ' + content);
  
  // 작업이 완료되어 파일이 더 이상 필요 없다고 가정
  // 임시 파일 제거
  tempFile.setTrashed(true);
  
  // 삭제 확인
  Logger.log('Temporary file deleted');
}
```

이 스크립트를 실행하면 다음과 같은 출력이 나옵니다:

```
Temporary file created: [생성된 임시 파일의 URL]
Content of tempFile: Temporary content
Temporary file deleted
```

이 예제 스크립트는 임시 파일의 생성, 해당 내용을 읽는 작업을 수행하고, 마지막으로 파일을 제거하여 정리하는 절차를 보여줍니다.

## 심층 분석
소프트웨어 개발에서 임시 파일을 생성하는 개념은 파일 관리 개념 자체만큼이나 오래됐습니다. 전통적인 파일 시스템에서 임시 파일은 종종 지정된 temp 디렉토리에 생성되며, 대규모 데이터셋 정렬, 웹 애플리케이션의 세션 데이터 보관 또는 파일 변환 프로세스 중 데이터 조각 저장과 같은 다양한 중간 프로세스에 중요합니다.

Google Apps Script에서 임시 파일을 생성하는 과정은 Google Drive의 인프라를 활용합니다. 이는 클라우드 기반 파일 관리와 전통적인 프로그래밍 개념의 흥미로운 결합을 제공합니다. 그러나 Google Drive가 부과하는 할당량 제한을 고려할 때, 이 방식으로 Google Drive에서 임시 파일을 생성하는 것은 제한 사항과 비용이 없지 않습니다. 또한, 로컬 파일 시스템에 비해 Google Drive에 네트워크를 통한 접근 지연은 고성능 애플리케이션에 있어 중요한 요소가 될 수 있습니다.

대안으로, 개발자들은 계산 중 임시 저장이 필요한 소규모 데이터 셋에 Google 시트를 고려하거나, 고성능 읽기/쓰기 작업과 더 큰 저장 용량을 요구하는 애플리케이션에 Google Cloud Storage를 고려할 수 있습니다. 이러한 각각의 솔루션은 지연 시간, 저장 한계, Google Apps Script에서의 사용 용이성과 관련하여 다른 절충안을 제공합니다. 궁극적으로, 선택은 애플리케이션의 특정 요구 사항과 그것이 운영되는 기존 인프라에 따라 달라집니다.
