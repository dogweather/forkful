---
title:    "Fish Shell: 디렉토리가 존재하는지 확인하기"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# 왜

디렉토리가 존재하는지 확인하는 것의 성능 향상, 조건부 쉘 스크립팅 및 디렉토리 관리에 대한 이해를 위해서입니다.

## 어떻게

```Fish Shell```을 사용하여 디렉토리가 존재하는지 확인하는 방법을 알아보겠습니다.

### 디렉토리 존재 여부 확인하기

"ls" 명령어를 사용하여 디렉토리 내에 존재하는 파일 및 디렉토리를 나열할 수 있습니다. 이 명령어를 "-d" 옵션과 함께 사용하면 현재 디렉토리의 존재 여부를 확인할 수 있습니다.

```
$ ls -d my_directory
```

위의 코드를 실행하면 "my_directory"라는 디렉토리가 존재한다면 디렉토리 이름이 출력될 것이고, 존재하지 않는다면 오류 메시지가 출력될 것입니다. 따라서 이 방법을 사용하면 디렉토리가 존재하는지 바로 알 수 있습니다.

### 조건부 쉘 스크립팅을 이용한 디렉토리 존재 여부 확인하기

"if" 문을 사용하여 디렉토리가 존재하는지 여부를 확인할 수 있습니다.

```
if test -d my_directory
    echo "my_directory exists"
else
    echo "my_directory does not exist"
end
```

위의 코드를 실행하면 "my_directory"라는 디렉토리가 존재한다면 "my_directory exists"가 출력될 것이고, 존재하지 않는다면 "my_directory does not exist"가 출력될 것입니다.

### 디렉토리 존재 여부에 따른 다른 작업 수행하기

"&&"를 사용하여 디렉토리가 존재할 경우 다른 작업을 수행하도록 할 수 있습니다.

```
test -d my_directory && echo "my_directory exists"
```

위의 코드를 실행하면 만약 "my_directory"라는 디렉토리가 존재한다면 "my_directory exists"가 출력될 것입니다.

## 깊이 파고들기

디렉토리 존재 여부를 확인하는 것은 간단한 작업처럼 보이지만, 실제로는 디렉토리 관리에서 매우 중요한 역할을 합니다. 디렉토리가 존재하는지 확인하는 것은 프로그래밍에서 디렉토리를 생성하거나 삭제할 때, 파일을 읽거나 쓸 때, 파일이 저장되는 디렉토리를 찾을 때 등 다양한 상황에서 필요합니다.

또한, 디렉토리 존재 여부를 확인하는 것은 보다 복잡한 프로그램에서 조건문으로 사용할 수 있어 유용합니다. 예를 들어, 만약 디렉토리가 존재하면 담당자에게 알림 메시지를 보내거나, 존재하지 않는 경우에는 디렉토리를 생성하는 스크립트를 실행하는 등의 작업을 할 수 있습니다.

## 관련 링크

- [Fish Shell 공식 사이트](https://fishshell.com/)
- [Fish Shell GitHub 저장소](https://github.com/fish-shell/fish-shell)
- [Fish Shell 설치 가이드](https://fishshell.com/docs/current/index.html#installing-fish)