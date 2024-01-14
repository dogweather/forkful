---
title:                "Fish Shell: 임시 파일 생성하기"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍을 하는 사람들이 일시적인 파일을 생성하는 이유는 여러 가지가 있습니다. 가령, 임시적으로 사용되는 데이터를 저장하기 위해서 일시적인 파일을 생성하거나, 파일을 사용하고 나서 삭제하면서 메모리를 절약하기 위해서입니다.

## 만드는 방법

Fish Shell의 `mktemp` 명령어를 사용하여 일시적인 파일을 생성할 수 있습니다. 아래는 그 예시입니다.

```Fish Shell
set my_file (mktemp -u) # 임시 파일 이름 생성
touch $my_file # 임시 파일 생성
echo "임시 파일을 생성하였습니다." > $my_file # 파일에 내용 쓰기
cat $my_file # 파일 내용 확인
rm $my_file # 파일 삭제
```

위의 예시 코드에서 `mktemp`는 임시 파일 이름을 생성할 때 사용하는 명령어이며, `-u` 옵션은 중복되지 않는 파일 이름을 생성하도록 합니다. `touch` 명령어를 사용하여 생성된 임시 파일을 생성 후, 원하는 내용을 `echo`로 파일에 쓴 뒤 `cat` 명령어로 파일 내용을 확인할 수 있습니다. 마지막으로 `rm` 명령어로 파일을 삭제할 수 있습니다.

## 깊이 파고들기

임시 파일을 생성하는 방식은 여러 가지가 있을 수 있으며, `mktemp` 명령어 외에도 `mktemp -d` 명령어를 사용하여 디렉토리를 생성할 수도 있습니다. 또한, 생성된 임시 파일의 이름을 바꾸지 않고 사용하고 싶은 경우 `mktemp -k`를 사용하여 임시 파일을 생성할 수 있습니다. 여러분의 프로그램이 종료되더라도 임시 파일은 지속되기 때문에, `trap` 명령어를 사용하여 프로그램이 종료될 때 파일을 자동으로 삭제하도록 설정할 수도 있습니다.

## 또 다른 정보 보기

- Official Fish Shell Documentation: https://fishshell.com/docs/current/
- Understanding Temporary Files in Programming: https://opensource.com/article/20/4/temporary-files-programming