---
title:                "컴퓨터 프로그래밍: 커맨드 라인 인수 읽기"
html_title:           "Bash: 컴퓨터 프로그래밍: 커맨드 라인 인수 읽기"
simple_title:         "컴퓨터 프로그래밍: 커맨드 라인 인수 읽기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

무엇이 바로 Bash 프로그래밍을 배워야 할 이유일까요? 바로 커맨드 라인 인자를 읽는 것입니다. 이 기능은 이제까지 배웠던 다른 언어에서는 볼 수 없었던 다양한 기능을 사용할 수 있게 해줍니다. 따라서 우리는 굳이 프로그래밍이나 개발을 할 때마다 복잡한 IDE를 사용하지 않고도 편리하게 프로그램을 실행할 수 있습니다.

## 사용 방법

커맨드 라인 인자를 읽는 방법은 아주 간단합니다. 우선, 명령어 뒤에 공백을 두고 "```-옵션```"과 같은 형식으로 추가 인자를 넣어줍니다. 그리고 이 명령어와 함께 함께 입력된 인자들은 자동으로 "```$@```"라는 변수에 저장됩니다. 이제 이 변수를 활용하여 원하는 작업을 수행하면 됩니다. 아래는 예제 코드와 실행 결과입니다.

```Bash
# 인자가 존재하는지 확인하기
if [ $# -eq 0 ]; then
  echo "사용법: ./program -option"
  exit 1
fi

# 모든 인자 출력하기
echo "전달된 인자들: $@"

# 특정 인자 추출하여 사용하기
option="$1"
echo "넘겨받은 옵션: $option"
```

```
사용법: ./program -option
전달된 인자들: -option
넘겨받은 옵션: -option
```

## 깊게 들어가기

커맨드 라인 인자를 다루는 방법은 상황에 따라 다양합니다. 예를 들어, 여러 개의 인자를 받아서 각각 다른 작업을 수행하거나, 인자가 없을 때 실행되는 기본 동작을 지정하는 등 다양한 방법이 있습니다. 또한, "```getopt```"와 같은 라이브러리를 활용하면 더욱 복잡한 옵션 처리도 쉽게 할 수 있습니다. 더 자세한 정보는 관련 문서나 블로그를 참고하시기 바랍니다.

## 관련 자료

- [GNU Bash 공식 문서](https://www.gnu.org/software/bash/manual/bash.html)
- [유용한 Bash 스크립팅 튜토리얼](https://linuxhint.com/bash_scripting_basic/)
- [Bash 커맨드 라인 인자 처리하기](https://www.baeldung.com/linux/bash-command-line-arguments)