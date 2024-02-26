---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:11.151885-07:00
description: "Google Apps Script\uC5D0\uC11C \uB514\uB809\uD130\uB9AC\uC758 \uC874\
  \uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uB294 \uAC83\uC740 Google \uB4DC\uB77C\
  \uC774\uBE0C \uB0B4\uC758 \uD3F4\uB354 \uC874\uC7AC\uB97C \uAC80\uC99D\uD558\uB294\
  \ \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC885\uC885 \uD30C\uC77C \uBC0F \uB514\uB809\uD130\uB9AC\uB97C \uD504\uB85C\
  \uADF8\uB798\uBC0D\uC801\uC73C\uB85C \uAD00\uB9AC\uD560 \uB54C \uC624\uB958 \uB610\
  \uB294 \uC911\uBCF5 \uD3F4\uB354 \uC0DD\uC131\uC744 \uD53C\uD558\uAE30 \uC704\uD574\
  \ \uC774\uB7EC\uD55C \uAC80\uC0AC\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:51.593956-07:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uB514\uB809\uD130\uB9AC\uC758 \uC874\uC7AC\
  \ \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uB294 \uAC83\uC740 Google \uB4DC\uB77C\uC774\
  \uBE0C \uB0B4\uC758 \uD3F4\uB354 \uC874\uC7AC\uB97C \uAC80\uC99D\uD558\uB294 \uAC83\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC885\uC885 \uD30C\uC77C \uBC0F \uB514\uB809\uD130\uB9AC\uB97C \uD504\uB85C\uADF8\
  \uB798\uBC0D\uC801\uC73C\uB85C \uAD00\uB9AC\uD560 \uB54C \uC624\uB958 \uB610\uB294\
  \ \uC911\uBCF5 \uD3F4\uB354 \uC0DD\uC131\uC744 \uD53C\uD558\uAE30 \uC704\uD574 \uC774\
  \uB7EC\uD55C \uAC80\uC0AC\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Google Apps Script에서 디렉터리의 존재 여부를 확인하는 것은 Google 드라이브 내의 폴더 존재를 검증하는 것을 포함합니다. 프로그래머들은 종종 파일 및 디렉터리를 프로그래밍적으로 관리할 때 오류 또는 중복 폴더 생성을 피하기 위해 이러한 검사를 수행합니다.

## 방법:

Google Apps Script는 폴더에 대한 직접적인 "존재 여부" 메소드를 제공하지 않습니다. 대신, 특정 이름을 가진 폴더가 존재하는지 확인하기 위해 Google 드라이브의 검색 기능을 사용합니다. 다음은 단계별 예시입니다:

```javascript
// 디렉터리가 존재하는지 확인하는 함수
function checkIfDirectoryExists(directoryName) {
  // 지정된 이름과 일치하는 폴더의 컬렉션 검색
  var folders = DriveApp.getFoldersByName(directoryName);
  
  // 지정된 이름을 가진 폴더가 최소한 하나 존재하는지 확인
  if (folders.hasNext()) {
    Logger.log('디렉터리가 존재합니다.');
    return true;
  } else {
    Logger.log('디렉터리가 존재하지 않습니다.');
    return false;
  }
}

// 예시 사용
var directoryName = 'My Sample Folder';
checkIfDirectoryExists(directoryName);
```

샘플 결과:
```
디렉터리가 존재합니다.
```
또는
```
디렉터리가 존재하지 않습니다.
```

이 스크립트는 지정된 이름과 일치하는 사용자의 드라이브 내 모든 폴더를 검색하는 `getFoldersByName` 메소드를 활용합니다. 드라이브에서 이름이 유일하지 않기 때문에, 이 메소드는 `FolderIterator`를 반환합니다. 이 반복기(iterator)의 다음 항목(`hasNext()`)의 존재는 디렉터리가 존재함을 나타냅니다.

## 심층 분석

역사적으로, 웹 및 클라우드 환경에서의 파일 관리는 상당히 발전했습니다. Google Apps Script는 Google 드라이브를 위한 광범위한 API를 제공하여, 검색 및 확인 메커니즘과 같이 고급 파일 및 폴더 관리 작업을 가능하게 합니다. 하지만, 구글 드라이브가 같은 이름의 다수 폴더를 허용하는 것과 대조적으로, 많은 파일 시스템이 같은 디렉터리 내에서 유일한 이름을 강제하는 것과는 달리, 직접적인 존재 확인 부재는 주목할 만한 측면입니다.

이러한 맥락에서, `getFoldersByName` 메소드를 사용하는 것은 효과적인 우회 방법이지만, 중복된 이름을 가진 대규모 폴더가 존재하는 시나리오에서 비효율성을 도입할 가능성이 있습니다. 더 빠른 확인을 보장하기 위해 애플리케이션별 인덱싱이나 명명 규칙을 유지하는 대안적 접근 방식을 고려할 수 있습니다. 특히 성능이 중요한 관심사가 될 때는 더욱 그렇습니다.

Google Apps Script의 접근 방식이 단일 파일 시스템과 직접 연결된 프로그래밍 언어에서 파일 존재 확인에 비해 처음에는 간접적으로 보일 수 있지만, 클라우드 기반 파일 저장소의 복잡성을 처리할 필요성을 반영합니다. Google Apps Script를 사용하여 드라이브 관리를 하는 개발자들은 Google 드라이브의 강점과 한계를 최적화하는데 이러한 미묘한 차이를 고려해야 합니다.
