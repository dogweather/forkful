---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:17.135077-07:00
description: "\uBC29\uBC95: Google Apps Script\uB294 \uD2B9\uD788 Google Docs\uC640\
  \ Sheets \uB0B4\uC5D0\uC11C \uD14D\uC2A4\uD2B8\uB97C \uAC80\uC0C9\uD558\uACE0 \uAD50\
  \uCCB4\uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\
  \uB2E4. \uC544\uB798\uB294 \uB450 \uAC00\uC9C0 \uC608\uC2DC\uC785\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:56.380935-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uB294 \uD2B9\uD788 Google Docs\uC640 Sheets \uB0B4\uC5D0\
  \uC11C \uD14D\uC2A4\uD2B8\uB97C \uAC80\uC0C9\uD558\uACE0 \uAD50\uCCB4\uD558\uB294\
  \ \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## 방법:
Google Apps Script는 특히 Google Docs와 Sheets 내에서 텍스트를 검색하고 교체하는 간단한 방법을 제공합니다. 아래는 두 가지 예시입니다.

### Google Docs:
Google 문서에서 텍스트를 검색하고 교체하려면 주로 `DocumentApp` 클래스와 상호 작용합니다.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // 특정 구문 검색 및 교체
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// 사용법
searchReplaceInDoc();
```

이 코드 스니펫은 활성 Google 문서에서 `'searchText'`의 모든 발생을 `'replacementText'`로 교체합니다.

### Google Sheets:
마찬가지로 Google Sheets에서는 `SpreadsheetApp`을 사용하여 검색 및 교체 작업을 수행할 수 있습니다:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // 현재 활성 시트에서 검색 및 교체하기
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// 사용법
searchReplaceInSheet();
```

이 예에서 `createTextFinder('searchText')`는 활성 시트에서 'searchText'를 검색하고, `replaceAllWith('replacementText')`는 모든 발생을 'replacementText'로 교체합니다.

## 심층 분석
Google Apps Script의 검색 및 교체 기능은 웹 기반의 특성에 크게 영향을 받아 스크립트가 다양한 Google 앱스에서 문제없이 텍스트를 조작할 수 있게 합니다. 역사적으로, 이 기능은 Perl 및 Python과 같은 언어에서 정규 표현식 및 문자열 함수를 사용한 프로그래밍에서 텍스트 처리 및 조작의 넓은 맥락에서 비롯되었습니다.

Google Apps Script의 검색 및 교체 기능은 간단한 대체 작업에 대해서는 강력하나, 일부 다른 언어에서 찾을 수 있는 전체 정규 표현식 기능은 부족합니다. 예를 들어, Google Sheets에서 `createTextFinder`에서 기본 정규 표현식을 사용할 수 있지만, Perl이나 Python에 비해 복잡한 패턴 매칭 및 조작 옵션이 제한적입니다.

보다 고급 텍스트 처리 작업이 필요한 경우, 프로그래머는 Google 문서나 시트 콘텐츠를 외부에서 처리할 수 있는 형식으로 내보내거나, Google Apps Script를 사용하여 보다 정교한 텍스트 조작 기능을 제공하는 외부 API 또는 서비스를 호출할 수 있습니다.

이러한 제한에도 불구하고, Google Apps의 생태계 내에서 일반적인 검색 및 교체 작업에 대해 Google Apps Script는 간단하고 효율적이며, Google의 생산성 도구 모음 내에서 자동화 및 스크립팅의 필요에 맞게 조정된 매우 통합 가능한 솔루션을 제공합니다.
