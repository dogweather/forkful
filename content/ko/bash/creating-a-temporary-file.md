---
title:                "임시 파일 만들기"
html_title:           "Bash: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

임시 파일을 생성하는 것은 우리가 직면한 문제를 해결하기 위한 일시적인 방법입니다. 프로그래머들은 임시 파일을 생성하는 이유는 먼저 작업 중인 파일이 아니라 일련의 명령을 실행하기 위해 사용하기 위해서입니다.

## 사용 방법:

Bash 스크립트에서 임시 파일을 생성하는 방법은 다음과 같습니다.
```bash
# Create a temporary file using the mktemp command
temp_file=$(mktemp) 

# Write to the temporary file
echo "Hello, World!" > $temp_file 

# Read the contents of the temporary file
cat $temp_file 

# Note: The temporary file will automatically be deleted when the script exits.
```
출력 결과:
```
Hello, World!
```

## 깊이 들어가기:

### 역사적 배경:

임시 파일 생성의 역사는 오래되었습니다. 과거에는 프로그래머들이 작업하는 파일을 자체적으로 생성했지만, 이는 동시에 여러 작업을 실행할 때 문제를 일으킬 수 있었습니다. 그래서 현재는 운영 체제가 이러한 문제를 해결하기 위해 임시 파일을 제공하고 있습니다.

### 대안:

Bash 스크립트에서 임시 파일을 생성하는 다른 방법으로는 쉘 변수를 사용하는 것이 있습니다. 하지만 이는 보안 측면에서 적절하지 않을 수 있습니다. 또 다른 대안으로는 tempfile이라는 명령이 있습니다. 하지만 이 명령은 사용 용도가 제한적이므로 일반적으로는 mktemp를 사용하는 것이 더 좋습니다.

### 구현 세부사항:

임시 파일 생성 명령은 보통 운영 체제에서 제공되는 것입니다. 그래서 Bash 스크립트에서는 이 명령을 사용하기만 하면 됩니다. mktemp 명령은 /tmp/ 디렉토리에 임시 파일을 생성하고 해당 파일의 경로를 출력합니다.

## 참고 자료:

- [Bash Manual - Temporary Files](https://www.gnu.org/software/bash/manual/html_node/Temporary-Files.html)
- [Linuxize - Creating Temporary Files and Directories in Bash](https://linuxize.com/post/creating-temporary-files-and-directories-in-bash/)