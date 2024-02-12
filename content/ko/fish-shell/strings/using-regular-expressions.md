---
title:                "정규 표현식 사용하기"
aliases:
- /ko/fish-shell/using-regular-expressions.md
date:                  2024-02-03T19:16:58.982339-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

Fish Shell에서의 정규 표현식(regex)은 특정 패턴을 기반으로 문자열을 검색, 일치시키고 조작할 수 있게 해줍니다. 프로그래머는 입력 검증, 파싱 및 텍스트 처리와 같은 작업에 복잡한 텍스트 패턴을 특정하는 간결하고 강력한 방법을 제공하기 때문에 regex를 활용합니다.

## 사용 방법:

Fish Shell 자체는 regex를 위한 내장 명령어를 가지고 있지 않지만, `grep`, `sed`, `awk`와 같이 regex를 지원하는 외부 명령어를 효과적으로 사용하여 스크립트에 regex 작업을 통합할 수 있습니다.

### `grep`을 사용한 기본 패턴 매칭
파일에서 패턴에 일치하는 줄 검색하기:

```fish
grep '^[0-9]+' myfile.txt
```

이 명령어는 `myfile.txt`에서 하나 이상의 숫자로 시작하는 라인을 찾습니다.

### `sed`를 사용한 추출 및 교체
파일에서 전화번호 추출하기:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' 연락처.txt
```

`data.txt`에서 "foo"의 모든 발생을 "bar"로 교체하기:

```fish
sed 's/foo/bar/g' data.txt
```

### 기본 Regex를 위한 `string` 사용
Fish Shell의 `string` 명령어는 매치 및 교체와 같은 간단한 regex 작업을 지원합니다:

문자열에서 패턴 매치하기:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
출력:
```
3.1.2
```

'fish' 다음에 오는 숫자를 'X.X.X'로 교체하기:

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
출력:
```
Welcome to fish X.X.X
```

### `awk`를 사용한 고급 매칭
첫 번째 열이 특정 패턴과 일치하는 경우 두 번째 열의 데이터를 출력하기:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

이 명령은 `datafile`에서 첫 번째 열이 "a"로 시작하고 하나 이상의 숫자가 뒤따르는 줄을 찾고, 두 번째 열을 출력합니다.

이러한 외부 명령어를 통합함으로써, Fish Shell 프로그래머는 복잡한 텍스트 조작 작업을 위해 정규 표현식의 전체 기능을 활용할 수 있게 되어, 쉘의 기본 기능을 강화할 수 있습니다.
