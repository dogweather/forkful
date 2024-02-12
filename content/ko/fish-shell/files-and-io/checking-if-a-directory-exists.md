---
title:                "디렉토리가 존재하는지 확인하기"
aliases:
- /ko/fish-shell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:31.793692-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?
Fish Shell에서 디렉토리의 존재 여부를 확인하는 것은, 스크립트가 디렉토리 구조의 존재 또는 부재에 기반한 결정을 내릴 수 있게 하여, 조건부 파일 작업, 로깅, 또는 환경 설정과 같은 작업을 가능하게 합니다. 이 기법은 파일시스템과 예측 가능한 방식으로 상호 작용하는 견고한 스크립트를 작성하기 위해 필수적입니다.

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
