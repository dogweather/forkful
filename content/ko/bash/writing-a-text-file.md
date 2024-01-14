---
title:    "Bash: 텍스트 파일 작성하기"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것이 왜 중요한지에 대해 알아보겠습니다. 텍스트 파일은 컴퓨터 프로그래밍에서 매우 중요한 역할을 합니다. 이는 우리가 작성한 코드 또는 데이터를 저장하기 위해 사용되며, 나중에도 접근하여 필요한 작업을 할 수 있도록 도와줍니다. 또한 텍스트 파일은 많은 애플리케이션에서 필수적인 구성 요소이기도 합니다.

## 어떻게

텍스트 파일을 작성하는 방법은 매우 간단합니다. 우선 텍스트 에디터 프로그램을 열어 새로운 파일을 만듭니다. 이제 원하는 내용을 작성하고, 파일을 저장할 때 파일의 확장자를 .txt로 설정합니다. 예를 들어, "hello.txt" 같은 형식으로 저장할 수 있습니다.

```Bash
# 파일 열기
vi hello.txt

# 내용 작성 후 저장
Hello World!
:wq
```
위 코드에서 `vi`는 텍스트 에디터 프로그램의 이름이며, `:wq`는 저장하고 나가는 명령어입니다. 위 코드를 실행하면 "Hello World!"라는 내용이 포함된 "hello.txt" 파일이 생성됩니다.

## 깊게 들어가기

텍스트 파일을 작성하는 데 사용하는 것은 매우 간단하다고 했지만, 사실 이 내용을 살펴보면 많은 일이 일어나는 것을 알 수 있습니다. 예를 들어, 우리가 "Hello World!"라는 내용을 작성했는데, 실제로는 이 내용이 01001000 01100101 01101100 01101100 01101111 00100000 01010111 01101111 01110010 01101100 01100100라는 이진수로 저장되고 있습니다.

이는 컴퓨터가 이진수로 정보를 저장하고 전송하기 때문입니다. 따라서 우리가 작성한 내용도 결국 컴퓨터에서는 이진수로 인식되어 저장됩니다.

많은 유용한 도구들도 텍스트 파일을 사용합니다. 예를 들어, Bash 프로그래밍 언어에서는 텍스트 파일을 사용하여 우리가 작성한 스크립트 파일을 저장하고 실행할 수 있습니다. 프로그래밍 작업 뿐만 아니라, 일상적인 문서 작업에서도 텍스트 파일은 굉장히 유용하게 사용됩니다.

## 참고

- [Learn Shell](https://learnshell.org/) 
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [The Power of the Command Line](https://lifehacker.com/5633909/who-needs-a-mouse-learn-to-use-the-command-line-for-almost-anything)