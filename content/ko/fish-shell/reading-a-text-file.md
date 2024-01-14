---
title:    "Fish Shell: 텍스트 파일 읽기"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것은 프로그래밍에서 매우 중요합니다. 텍스트 파일은 업무나 개인 프로젝트에서 사용되는 많은 정보를 포함하고 있기 때문에, 이를 읽는 것은 프로그래머에게 매우 유용한 기술입니다.

## 어떻게

```Fish Shell```은 텍스트 파일을 읽고 파싱하는 데 매우 강력한 도구입니다. 다음은 텍스트 파일을 읽어서 각 줄을 출력하는 간단한 예제 코드입니다.

```Fish Shell
set lines (cat testfile.txt)
for line in $lines
	echo $line
end
```

위 코드를 실행하면, 텍스트 파일의 각 줄이 한 줄씩 출력됩니다. 이는 매우 간단하지만 유용한 방법입니다. 또한, ```cat```이 아닌 ```sed```, ```grep```, ```awk``` 등 다른 명령어를 사용하여 텍스트 파일을 파싱할 수도 있습니다.

## 깊이 파고들기

텍스트 파일을 읽는 데에는 여러 가지 방법이 있습니다. 위 예제에서는 텍스트 파일을 한 줄씩 읽어서 처리하는 방법을 보여주었지만, 더 복잡한 방법도 많이 있습니다. 예를 들어, 특정 패턴을 찾거나 특정 단어를 바꾸는 등의 작업을 할 수 있습니다. 더 깊이있는 정보는 [Fish Shell 공식 문서](https://fishshell.com/docs/current/)에서 찾아볼 수 있습니다.

## 참고 자료

- [텍스트 파일을 읽는 간단한 방법](https://stackoverflow.com/questions/12797193/how-to-read-a-file-line-by-line-into-a-list)
- [```Fish Shell``` 공식 문서](https://fishshell.com/docs/current/)