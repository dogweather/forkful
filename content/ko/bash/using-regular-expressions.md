---
title:                "정규 표현식 사용하기"
html_title:           "Bash: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규식(regular expression)은 특정 패턴이 문자열에 포함되어 있는지 여부를 확인할 때 사용하는 강력한 도구입니다. 프로그래머들은 효율적인 문자열 검색, 치환 등을 위해 정규식을 활용합니다.

## 사용법:

여러분이 정규식을 활용하여 한 문자열에서 다른 문자열을 찾아내는 방법을 알려 드리겠습니다.

```Bash
echo "Welcome to Bash programming!" | grep -o "Bash"
```

이 명령어를 실행하면 #### 창에 'Bash'라는 단어가 표시됩니다. `grep` 명령은 입력받은 문자열에서 검색하고자 하는 패턴을 찾는 역할을 합니다. `-o` 옵션을 사용하면 일치하는 정확한 단어만 표시하게 됩니다.

## 깊이 알기:

정규식은 1950년대에 이론화되어, 다양한 언어와 시스템에서 반복적 작업을 처리하는 데 사용됩니다. `awk` 또는 `sed` 같은 방식도 문자열 처리를 위해 사용되지만, `grep`은 가장 일반적인 방식입니다.

정규식의 구현은 대체로 최소화 DFA(Deterministic Finite Automaton를 활용한 컴파일과정을 통해 이루어집니다. 이는 복잡하지만, 자세한 내용은 우리 토픽 범위를 벗어나므로 이 부분은 패스하겠습니다.

## 참고 자료:

- [정규 표현식 쿡북 2판](https://www.hanbit.co.kr/store/books/look.php?p_code=B4300598719)
- [Bash에서 정규식 사용하기](https://wiki.kldp.org/wiki.php?page=RegularExpression%20in%20Bash)
- [정규식에 대한 MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)