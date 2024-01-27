---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
정규 표현식이란 문자열에 있는 특정 패턴을 찾고 조작하기 위한 문자 조합입니다. 프로그래머들이 사용하는 이유는 데이터 검증, 검색, 텍스트 처리 등 복잡한 문자열 작업을 효율적으로 다루기 위해서입니다.

## How to:
```Bash
# 문자열에서 간단한 매칭 찾기
echo "Hello World" | grep "World"

# 출력: World

# 정규 표현식을 사용해서 이메일 형식 찾기
echo "my.email@example.com" | grep -E "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"

# 출력: my.email@example.com

# sed를 이용한 검색 및 대체 (s/찾을패턴/새로운문자열/)
echo "The rain in Spain" | sed 's/rain/sun/'

# 출력: The sun in Spain

# 파일에서 패턴에 일치하는 줄 찾기
grep -E "^\s*error" /var/log/syslog

# 패턴에 일치하는 줄 여러 개가 출력될 수 있음
```

## Deep Dive
1980년대 초, Ken Thompson이 정규 표현식을 UNIX의 ed 에디터에 도입했습니다. 정규 표현식을 활용한 유명한 도구로는 grep, sed, awk가 있습니다. Perl, Python 같은 프로그래밍 언어도 정규 표현식을 내장하고 있습니다. 정규 표현식은 POSIX 표준과 Perl 호환 표현식인 PCRE (Perl Compatible Regular Expressions)로 나뉩니다.

## See Also
- GNU grep 매뉴얼: https://www.gnu.org/software/grep/manual/grep.html
- sed 사용법: https://www.gnu.org/software/sed/manual/sed.html
- 정규 표현식에 대한 더 깊은 학습: https://www.regular-expressions.info/
- 온라인 정규 표현식 테스터: https://regexr.com/
