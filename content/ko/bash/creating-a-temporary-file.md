---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

임시 파일 생성이란 프로그램이 작동하는 동안 일시적으로 데이터를 저장하는 파일을 만드는 작업입니다. 프로그래머들은 데이터를 순차적으로 처리하거나 디스크 공간을 절약할 목적으로 이를 사용합니다.

## 어떻게:

임시 파일을 생성하고 사용하는 방법은 다음과 같습니다.

```Bash
# 임시 파일 생성
tempfile=$(mktemp)

# 임시 파일에 데이터 쓰기
echo "임시 데이터" > $tempfile

# 임시 파일 읽기
cat $tempfile

# 임시 파일 삭제
rm $tempfile
```
샘플 출력:

```Bash
임시 데이터
```

## 깊이 분석

1. **역사적 맥락:** `mktemp` 명령은 처음에는 BSD 계열의 UNIX 시스템에서 사용되기 시작했습니다. 이후에 GNU coreutils에 포함되어 리눅스에서 널리 사용되게 되었습니다.

2. **대안:** `tempfile` 명령은 `mktemp`의 오래된 버전으로, 권장되지 않습니다. `mktemp`는 보안상 더 안전하기 때문입니다.

3. **구현 세부 정보:** `mktemp`는 중복되지 않는 임시 파일 이름을 생성합니다. 이는 파일이 이미 존재하는 경우 충돌을 피하기 위함입니다.

## 참고 자료

- [mktemp man page](https://man7.org/linux/man-pages/man1/mktemp.1.html): `mktemp` 명령에 대한 자세한 정보를 얻을 수 있는 공식 사용자 메뉴얼.


- [Discussion on Stack Overflow](https://stackoverflow.com/questions/4632028/how-to-create-a-temporary-directory): 임시 디렉터리를 만드는 법에 대한 토론.