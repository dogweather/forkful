---
title:    "Fish Shell: 디렉토리가 존재하는지 확인하기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

모든 프로그래머들은 자신의 코드가 오류 없이 잘 작동하길 바랍니다. 그중 한 가지 방법이 디렉토리의 존재 유무를 확인하는 것입니다. 디렉토리 유무 체크하는 법을 배워보세요!

## 어떻게

Fish Shell에서 디렉토리가 존재하는지 확인하는 방법은 간단합니다. ```test -d 디렉토리주소```라는 명령어를 사용하면 됩니다. 아래의 예제를 통해 살펴보겠습니다.

```
test -d /Users/사용자명/Documents
and echo "디렉토리가 존재합니다"
```

위 코드는 디렉토리가 존재할 경우 "디렉토리가 존재합니다"라는 메시지를 출력합니다. 만약 디렉토리가 존재하지 않는다면 아무런 출력도 하지 않습니다.

## 깊이 들어가보기

만약 여러분이 디렉토리가 존재하는지 여부를 확인하기만 하고 싶지 않다면, 다른 옵션을 사용할 수도 있습니다. ```test``` 명령어에는 여러 옵션이 있으며, ```-d``` 옵션 외에도 ```-f```, ```-e```, ```-L``` 등이 있습니다.

- ```-f``` 옵션은 파일이 존재하는지 확인합니다.
- ```-e``` 옵션은 로컬 파일/디렉토리가 존재하는지 확인합니다.
- ```-L``` 옵션은 심볼릭 링크가 존재하는지 확인합니다.

따라서 여러분은 ```test -f 파일주소```와 같은 방식으로 파일 존재 여부를 확인할 수 있습니다.

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell: Using Test Expressions](https://fishshell.com/docs/current/commands.html#test)
- [How to Check if a File or Directory Exists using Fish Shell](https://linuxize.com/post/how-to-check-if-a-file-or-directory-exists-using-fish-shell/)