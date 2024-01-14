---
title:                "Bash: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜 Standard Error를 사용하는가?

프로그래밍에서 오류는 피할 수 없는 부분입니다. 때로는 예기치 못한 동작이 발생하거나 프로그램이 중단되는 경우가 있습니다. 이러한 오류를 디버깅하고 발견하기 위해서는 우리는 적절한 방법으로 오류 메시지를 출력해야 합니다. 여기서 Standard Error가 필요한데, 이것은 표준 출력(Standard Output)과는 다르게 프로그램의 오류 메시지를 출력하는 채널입니다. 즉, Standard Error를 사용하면 오류 메시지를 더 효과적으로 디버깅할 수 있습니다.

# Standard Error 사용하는 방법

Bash를 사용해서 Standard Error를 출력하는 방법은 매우 간단합니다. 우선, `>>` 기호를 사용하여 오류 메시지를 출력할 파일을 지정합니다. 다음은 오류 메시지를 출력하는 예시 코드입니다.

```Bash
echo "이것은 오류 메시지입니다." >> error.log
```

위 코드를 실행하면 "이것은 오류 메시지입니다."라는 문자열이 `error.log`라는 파일에 새로 추가됩니다.

# 더 깊이 들어가기

Bash에서 Standard Error를 출력하는 가장 일반적인 방법은 `2>` 기호를 사용하는 것입니다. 이 기호는 우리가 오류 메시지를 출력할 파일을 지정하는 것이 아니라, 오류 메시지 자체를 지정하는 것입니다. 즉, 다음과 같이 사용됩니다.

```Bash
echo "이것은 오류 메시지입니다." 2> error.log
```

이 예제에서는 오류 메시지를 직접 `error.log` 파일에 출력합니다. 이외에도 Bash에서는 오류 메시지를 별도의 파일에 저장하는 방법 외에도 다양한 방법으로 Standard Error를 활용할 수 있습니다. 더 많은 정보는 Bash 공식 문서나 인터넷 자료를 참고하시기 바랍니다.

# 더 찾아보기

- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Bash 스크립트 튜토리얼 (번역)](https://wikidocs.net/book/587)
- [Bash 생활백서 (번역)](https://devh.kr/3)