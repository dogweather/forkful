---
title:    "Bash: 표준 오류에 쓰는 방법"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜

*표준 오류에 쓰기를 하는 부분에 대해 더 알고 싶으면 그 대안을 제공하는 다른 스트림 또는 콘솔의 상세한 정보가 필요할 수 있습니다. 이는 디버깅이 필요한 코드에서 특히 중요하며, 콘솔에 출력되는 모든 정보를 알고 싶을 때 유용할 수 있습니다.*

## 어떻게

```Bash
echo "이것은 표준 오류 출력입니다." 1>&2
```

위의 예시에서 볼 수 있듯이, 표준 오류는 기본적으로 2번 스트림에 연결되어 있으며, 표준 오류 출력을 원하는 명령어나 스크립트의 뒤에 `1>&2`를 추가하여 사용할 수 있습니다. 이렇게 하면 표준 출력이 아닌 표준 오류로 출력됩니다.

## 깊이 파고들기

표준 오류의 사용은 디버깅 외에도 유용할 수 있습니다. 예를 들어, 스크립트를 실행하다가 오류가 발생했을 때, 표준 오류에 메시지를 출력하여 사용자에게 알리는 것이 좋습니다.

또한, 표준 오류를 사용해 로그 파일을 작성할 수도 있습니다. 동일한 스크립트를 여러 번 실행하면서 표준 오류를 콘솔이 아닌 파일로 리디렉션하여 오류 메시지를 로그 파일에 저장할 수 있습니다.

# 참고 자료

* See Also (참고 자료)
    * [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_02_03.html)
    * [Bash scripting cheatsheet](https://devhints.io/bash)
    * [Bash scripting tutorial for beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)