---
title:    "Bash: 디버그 출력 프린트"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 활용하는 이유는 코드의 문제점을 파악하고 수정하기 쉽게 하기 위함입니다.

## 하는 법

디버그 출력을 위해서는 `echo`를 사용하면 됩니다. 예를 들면:

```Bash
#!/bin/bash
name="김철수"
echo "$name" # OUTPUT: 김철수
```

만약 이 코드를 실행하면 `김철수`가 출력됩니다. 이는 디버그를 위해 변수 `name`이 어떤 값을 가지고 있는지 확인하기 위한 것입니다.

## 깊이 파고들기

디버그 출력은 코드를 디버그 하는 데 가장 일반적인 방법 중 하나입니다. 그러나, 너무 많은 디버그 출력은 코드의 실행 속도를 느리게 할 수 있고, 가독성을 떨어뜨릴 수 있습니다. 따라서 디버그 출력을 사용할 때는 이점과 단점을 고려하여 적절히 활용하는 것이 중요합니다.

## 관련 글

[디버그 출력에 관한 중요한 팁들](https://www.freesoftwarereview.com/ways-to-do-debug-printing-in-bash/)  
[Bash 스크립트를 디버그하는 방법](https://linuxconfig.org/how-to-debug-a-shell-script)  
[Bash 스크립트를 디버그하는 방법 - 디버그 출력 중심](https://www.codeproject.com/Articles/13309/Debugging-a-Shell-Script)