---
title:                "Bash: 표준 에러 출력하기"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜
이 글을 쓰는 이유는 무엇일까요? "표준 오류"에 대해 직접적으로 쓸 필요가 있는 이유는 무엇이 있을까요? 일반적인 경우에는 애플리케이션의 실행 과정에서 오류가 발생할 수 있습니다. 이 오류를 적절히 처리하지 않으면 사용자는 애플리케이션에 대한 정보를 얻을 수 없습니다. 따라서 오류 메시지를 출력하는 것은 매우 중요합니다.

## 어떻게
이제 "표준 오류"에 대해 어떻게 쓰는지 살펴보겠습니다. 우선, 오류를 출력하기 위해 "데이터 스트림 리디렉션"을 사용해야 합니다. 이를 통해 출력되는 메시지가 화면이 아닌 파일로 전송될 수 있습니다. 예를 들어, 다음과 같이 사용할 수 있습니다:
```Bash
./my_app 2> error_log.txt
```
위의 코드는 "my_app" 애플리케이션의 실행 결과를 "error_log.txt" 파일에 저장합니다. 만약 오류가 발생하면, 해당 오류 메시지가 "error_log.txt" 파일에 출력됩니다.

## 깊이 들어가기
이제 더 깊이 들어가보겠습니다. 표준 오류를 출력하는 가장 일반적인 방법은 "echo"명령어를 사용하는 것입니다. 예를 들어,
```Bash
echo "An error has occurred" 1>&2
```
위의 코드는 "echo" 명령어를 사용하여 "표준 오류"로 "An error has occurred"라는 메시지를 출력합니다. 우리는 "1>&2"라는 리디렉션 연산자를 사용하여 메시지를 표준 오류로 전송합니다.

또 다른 유용한 방법은 "stderr" 명령어를 사용하는 것입니다. "stderr" 명령어는 오류 메시지를 출력할 때 사용할 수 있는 전역 변수입니다. 예를 들어,
```Bash
#!/bin/bash
echo "This is a normal message"
sleep 2
>&2 echo "An error has occurred"
```
위의 코드는 "이것은 일반적인 메시지입니다"라는 메시지를 출력한 다음, 2초 후에 "stderr" 명령어를 사용하여 "An error has occurred"라는 메시지를 표준 오류로 출력합니다.

## 참고
마지막으로, 더 많은 정보를 원한다면 다음 링크들을 참고하시기 바랍니다.
- [리디렉션 연산자에 대한 자세한 내용](https://ryanstutorials.net/linuxtutorial/piping.php)
- [오류 처리에 대한 좋은 설명](https://www.baeldung.com/linux/bash-standard-error)
- [stdout과 stderr에 대한 깊이있는 설명](https://guides.yooseunghui.net/programming/bash-shell-scripting/chapter-output.html)

## 참고 문서
- [리디렉션 연산자에 대한 자세한 내용](https://ryanstutorials.net/linuxtutorial/piping.php)
- [오류 처리에 대한 좋은 설명](https://www.baeldung.com/linux/bash-standard-error)
- [stdout과 stderr에 대한 깊이있는 설명](https://guides.yooseunghui.net/programming/bash-shell-scripting/chapter-output.html)