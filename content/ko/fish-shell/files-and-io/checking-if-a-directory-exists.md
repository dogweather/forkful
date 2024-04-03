---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:31.793692-07:00
description: "\uBC29\uBC95: Fish Shell\uC740 `test` \uBA85\uB839\uC5B4\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC \uD30C\uC77C \uD0C0\uC785\uACFC \uD2B9\uC131\uC744 \uD655\uC778\
  \uD569\uB2C8\uB2E4. \uC5EC\uAE30\uC5D0\uB294 \uB300\uC0C1\uC774 \uB514\uB809\uD1A0\
  \uB9AC\uC778\uC9C0 \uC5EC\uBD80\uB3C4 \uD3EC\uD568\uB429\uB2C8\uB2E4. \uB514\uB809\
  \uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD558\uAE30 \uC704\
  \uD55C \uAE30\uBCF8 \uD328\uD134\uC740 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:55.876462-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\uC740 `test` \uBA85\uB839\uC5B4\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uD30C\uC77C \uD0C0\uC785\uACFC \uD2B9\uC131\uC744 \uD655\uC778\uD569\uB2C8\uB2E4\
  ."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 방법:
Fish Shell은 `test` 명령어를 사용하여 파일 타입과 특성을 확인합니다. 여기에는 대상이 디렉토리인지 여부도 포함됩니다. 디렉토리가 존재하는지 확인하기 위한 기본 패턴은 다음과 같습니다:

```fish
if test -d /path/to/dir
    echo "디렉토리가 존재합니다"
else
    echo "디렉토리가 존재하지 않습니다"
end
```
샘플 출력:
```
디렉토리가 존재합니다
```

파일 및 디렉토리 작업을 더 간소화하기 위해, `fd`와 같은 외부 도구를 사용할 수도 있으나, 이는 존재 여부를 단순히 확인하는 것보다 파일 및 디렉토리를 찾는 데 더 일반적으로 사용됩니다. 그러나 이를 Fish 스크립팅과 결합하면 유용한 결과를 얻을 수 있습니다:

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "디렉토리가 존재합니다"
else
    echo "디렉토리가 존재하지 않습니다"
end
```

이 `fd` 예시는 지정된 깊이에서 디렉토리를 검색하고, `grep`은 일치하는 항목을 확인합니다. 이는 미묘한 검사에 다양하게 사용될 수 있지만, 존재 여부를 직접적으로 확인하는 목적으로는 Fish 내장 `test`를 사용하는 것이 효율적이고 간단합니다.
