---
title:                "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

왜 문자열을 소문자로 변환하는 것에 관심을 두어야 할까요? 이 기능은 왜 유용할까요? 일반적으로 많은 프로그래밍 언어에서는 문자열을 소문자로 변환하는 기능을 제공하고 있어서, 이 기능은 유용하게 사용될 수 있습니다.

## 사용 방법

이제 문자열을 소문자로 변환하는 방법을 알아보겠습니다. 아래의 코드 예제를 사용하여 간단한 방법을 설명하겠습니다.

```Bash
#!/bin/bash
text="Let's Convert This String To Lower Case!"
echo ${text,,}
```

위 코드에서 중요한 부분은 ```${text,,}``` 입니다. 이렇게 하면 변수인 ```text```에 저장된 문자열이 소문자로 변환되어 출력됩니다.

다른 방법으로는 ```tr``` 명령어를 사용하는 방법도 있습니다. 아래의 코드 예제를 통해 확인해보세요.

```Bash
#!/bin/bash
text="Let's Convert This String To Lower Case!"
echo $text | tr '[:upper:]' '[:lower:]'
```

위 코드에서는 ```tr``` 명령어를 사용해서 모든 대문자를 소문자로 변환하고, 결과를 출력하고 있습니다.

## 깊이 파고들기

이제 문자열을 소문자로 변환하는 방법을 보다 자세하게 살펴보겠습니다. 일반적으로 문자열을 소문자로 변환하기 위해서는 아래와 같이 코드를 작성합니다.

```Bash
text="This Is A Sample Text"
echo ${text,,}
```

여기서 사용된 ```,,```가 바로 문자열을 소문자로 변환하는 부분입니다.

또 다른 방법으로는 ```tr``` 명령어를 사용하는 방법도 있습니다. ```tr``` 명령어는 캐릭터 변환을 위해 사용되는 유틸리티 명령어입니다. 위에서 언급한 대로 모든 대문자를 소문자로 변환하기 위해서는 아래와 같은 코드를 작성할 수 있습니다.

```Bash
echo $text | tr '[:upper:]' '[:lower:]'
```

또 다른 방법으로는 ```sed``` 명령어를 사용하는 방법도 있습니다. ```sed``` 명령어는 스트림 편집기로서, 텍스트 패턴을 찾아서 그 패턴을 다른 텍스트로 치환하거나, 텍스트를 삭제 혹은 변형하는 기능을 제공합니다. 아래와 같은 코드를 사용하면 모든 대문자를 소문자로 변환할 수 있습니다.

```Bash
echo "$text" | sed 's/[A-Z]/\L&/g'
```

여기서 중요한 부분은 ```/g```입니다. 이 부분은 첫번째 일치하는 부분말고도 모든 일치하는 부분을 찾아서 변환하라는 의미입니다.

## 참고

마지막으로 ```tr``` 명령어와 관련된 몇 가지 참고 자료들을 소개해드리겠습니다.

- [GNU bash 공식 문서](https://www.gnu.org/software/bash/)
- [The Linux Documentation Project](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Shell Scripting 왕초보 프로그래밍 총정리](https://wikidocs.net/book/319)

## 참고 자료

- [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Tr Command in Linux with Examples](https://linuxhint.com/tr-command-linux-examples/) 
- [Sed Tutorial for Linux](https://likegeeks.com/linux-sed-tutorial/)