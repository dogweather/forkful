---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "Bash: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

이번 주제는 "패턴에 일치하는 문자를 삭제하는 방법"입니다. 주어진 작업을 처리하는 다양한 방법 중 하나로 이 기능을 알고 있으면 명령줄 환경에서 유용하게 사용할 수 있습니다.

## 사용 방법

예를 들어, "Hello, World!"라는 문자열에서 "o"를 삭제하는 코드는 다음과 같이 작성할 수 있습니다. 

```Bash
echo "Hello, World!" | sed 's/o//g'
```

이 코드는 sed (Stream EDitor) 라는 명령어를 사용하여 "s(o)"" 옵션을 통해 "o"를 빈 문자열로 바꿔주는 작업을 수행합니다. 따라서 출력된 결과는 "Hell, Wrld!"가 됩니다.

## 딥 다이브

패턴을 삭제하는 방식은 다양합니다. 여러분이 익숙한 명령어를 사용해도 되지만, 여기서는 sed를 사용하는 예를 들어보겠습니다. 

- 일반적인 형식: `sed 's/pattern/replacement/flags'`

이때 `pattern`은 삭제할 문자를 가리키며, `replacement`은 대체할 문자를 가리킵니다. `flags`에는 다양한 옵션을 추가하여 작업을 세부적으로 제어할 수 있습니다.

- 예시: `sed 's/John/Doe/'`

위의 예시에서는 "John"이라는 문자열이 "Doe"로 대체됩니다.

이 외에도 `grep`, `awk`, `tr` 등 다양한 명령어를 사용하여 패턴을 삭제할 수 있습니다. 기존에 사용하던 명령어로도 충분히 가능합니다. 다양한 명령어를 알고 있으면 상황에 맞게 유연하게 사용할 수 있습니다.

## 관련 정보

참고할 만한 관련 정보와 링크를 모아봤습니다.

- [Sed Command in Linux/Unix with Examples](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)
- [grep vs sed vs awk: what to use, when?](https://stackoverflow.com/questions/7383287/grep-vs-sed-vs-awk-what-to-use-when)
- [Linux sed Command Summary and Examples](https://www.guru99.com/sed-linux.html)

## 참고

- [Markdown Syntax Guide](https://www.markdownguide.org/basic-syntax/)
- [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)