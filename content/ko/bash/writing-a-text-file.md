---
title:                "Bash: 텍스트 파일 작성하기"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것에 참여하는 이유는 여러 가지가 있습니다. 예를 들어, 프로그래머가 되고 싶다면 여러분의 코드를 저장하기 위해서이며, 문서 작성에 필요하다면 이것은 매우 유용한 방법입니다. 또한 텍스트 파일은 운영체제에서 사용되는 설정 파일에도 적용될 수 있으며, 일부 텍스트 에디터에서는 다양한 형식의 파일을 작성할 수 있습니다.

## 방법

텍스트 파일을 작성하는 것은 매우 간단한 작업입니다. 먼저, 지정된 파일명의 새 파일을 만들고 "vim"과 같은 편집기를 사용하여 내용을 채웁니다. 이후, 파일을 저장하고 종료하면 해당 파일이 생성됩니다. 아래는 Bash 스크립트를 사용하여 새 텍스트 파일을 작성하는 예시입니다. 

```Bash
#!/bin/bash
# 새 파일 만들기
touch newfile.txt
# vim 편집기 열기
vim newfile.txt
# 내용 작성 후 :wq 입력하여 저장 및 종료
```

만약 파일의 내용을 확인하고 싶다면 "cat" 명령어를 사용하여 파일의 내용을 출력할 수 있습니다. 

```Bash
# 파일 내용 출력
cat newfile.txt
```

## 더 깊은 곳으로

텍스트 파일을 작성하는 것은 단순히 텍스트를 저장하는 것 이상입니다. 여러분은 파일의 확장자를 통해 어떤 형식의 파일인지를 지정할 수 있으며, 관련 소프트웨어를 통해 해당 파일을 이해하고 작동하게 할 수 있습니다. 또한 파일의 인코딩 방식이나 파일의 권한 등을 설정하여 파일을 보다 안정적으로 관리할 수 있습니다.

## 참고 자료

- [Cmd 커맨드 라인 인터페이스 문서](https://docs.microsoft.com/ko-kr/windows-server/administration/windows-commands/cmd)
- [리눅스 파일 권한 설정하기](https://d2.naver.com/helloworld/449936)
- [리눅스 용어사전 - 파일](http://www.linux.co.kr/article/view.do?article_id=1167)