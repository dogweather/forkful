---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:45.264145-07:00
description: "Google Apps Script\uC5D0\uC11C CSV(Comma-Separated Values, \uC27C\uD45C\
  \uB85C \uBD84\uB9AC\uB41C \uAC12) \uD30C\uC77C\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740\
  \ \uAC01 \uC904\uC774 \uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12\uC744 \uAC00\uC9C4\
  \ \uB370\uC774\uD130 \uB808\uCF54\uB4DC\uB97C \uB098\uD0C0\uB0B4\uB294 \uC77C\uBC18\
  \ \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uACE0, \uC218\uC815\uD558\uACE0,\
  \ \uC791\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 CSV\uAC00\u2026"
lastmod: '2024-03-11T00:14:28.482651-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C CSV(Comma-Separated Values, \uC27C\uD45C\uB85C\
  \ \uBD84\uB9AC\uB41C \uAC12) \uD30C\uC77C\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC740\
  \ \uAC01 \uC904\uC774 \uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12\uC744 \uAC00\uC9C4\
  \ \uB370\uC774\uD130 \uB808\uCF54\uB4DC\uB97C \uB098\uD0C0\uB0B4\uB294 \uC77C\uBC18\
  \ \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uACE0, \uC218\uC815\uD558\uACE0,\
  \ \uC791\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 CSV\uAC00\u2026"
title: "CSV \uD30C\uC77C\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Google Apps Script에서 CSV(Comma-Separated Values, 쉼표로 분리된 값) 파일을 다루는 것은 각 줄이 쉼표로 구분된 값을 가진 데이터 레코드를 나타내는 일반 텍스트 파일을 읽고, 수정하고, 작성하는 것을 포함합니다. 프로그래머들은 CSV가 간단한 텍스트 기반 데이터 교환 포맷으로 광범위하게 채택되어 있기 때문에 다른 애플리케이션, 데이터베이스, 또는 프로그래밍 언어 간 데이터를 쉽게 교환하기 위해 이 작업을 합니다.

## 방법:

### CSV 데이터 읽기

Google 드라이브에 저장된 파일에서 CSV 데이터를 읽으려면, 먼저 파일의 내용을 문자열로 가져온 후 파싱해야 합니다. Google Apps Script는 DriveApp 서비스를 사용하여 파일 콘텐츠 가져오기를 간단하게 만듭니다.

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // 실제 파일 ID로 교체
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // 각 행의 셀 로그
  }
}
```

### CSV 데이터 쓰기

CSV를 생성하고 쓰기 위해서는 쉼표로 구분된 값과 새 줄로 구성된 문자열을 만들고, 이를 저장하거나 내보내는 것을 포함합니다. 이 예제는 Google 드라이브에 새 CSV 파일을 생성하는 것을 보여줍니다.

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // 새 파일이 생성될 드라이브 폴더의 ID로 교체
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### 샘플 출력

CSV를 읽을 때 로우 셀을 로깅했을 때:

```plaintext
[John, 29, Engineer]
[Jane, 34, Designer]
```

쓰기를 할 때, "example.csv"라는 파일이 다음의 내용으로 생성됩니다:

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## 심층 탐구

역사적으로 CSV 파일은 단순함과 사람이 읽기 쉬움으로 인해 비프로그래머에게 접근하기 쉽고 빠른 데이터 검사 작업에 유용하다는 이유로 선호되었습니다. 그러나 Google Apps Script는 Google의 생태계 내에서 작동하며, Google 스프레드시트는 CSV 조작에 대한 강력하고 사용자 친화적인 대안으로 작동합니다. 스프레드시트는 데이터 편집을 위한 GUI뿐만 아니라 복잡한 수식, 스타일링 및 원시 CSV가 결여된 많은 기능을 지원합니다.

Google 스프레드시트가 제공하는 장점에도 불구하고, Google Apps Script에서 직접적인 CSV 조작은 외부 시스템이 CSV 형식의 데이터를 생성하거나 요구할 때, 특히 레거시 시스템과의 통합, 다른 애플리케이션에서 사용하기 위한 데이터 내보내기, 또는 Google 스프레드시트로 데이터를 공급하기 전에 전처리하는 경우와 같은 자동화 작업에서 중요합니다.

또한, Google Apps Script의 CSV 파일 작업 기능은 고급 인코딩 요구 사항에 대해 Utilities 서비스로 확장되거나 변환, 파싱, 또는 검증 작업을 위한 외부 API와 인터페이스될 수 있습니다. 하지만, 대규모 데이터 세트를 다루거나 복잡한 조작이 필요한 경우, 더 강력한 데이터 처리 기능을 위해 Google 스프레드시트 API를 활용하거나 BigQuery를 탐색하는 것을 고려하세요.

CSV의 인기에 기여하는 단순함에도 불구하고, 이러한 대안들은 Google Cloud 생태계 내에서 데이터를 다루기 위한 더 풍부한 기능 세트를 제공합니다.
