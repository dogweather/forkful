---
title:    "Gleam: 디렉토리의 존재 여부 확인하기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜 (Why)

디렉터리(Directory)가 존재하는지 확인하는 것이 왜 중요할까요? 여러분은 실제로 파일이 있는지 없는지를 빠르게 알고, 파일 시스템에 전체적인 사용을 파악하는 것이 매우 중요합니다.

# 어떻게 (How To)

우리는 ```Gleam`` 프로그래밍 언어를 사용하여 파일이 존재하는지 확인하는 방법을 알아보겠습니다. 먼저, 다음과 같이 ```fs``` 라이브러리를 이용하여 파일이 존재하는지 확인할 수 있습니다.

```
Gleam use fs

let path = "existed_directory"

if fs.exists(path) {
    Gleam.io.print("디렉터리가 존재합니다.")
} else {
    Gleam.io.print("디렉터리가 존재하지 않습니다.")
}
```

위의 예제를 실행하면 "디렉터리가 존재합니다." 또는 "디렉터리가 존재하지 않습니다." 라는 결과를 얻을 수 있습니다.

# 딥 다이브 (Deep Dive)

자세한 내용을 알고 싶다면, 다음의 링크를 참조해주세요.

- [Gleam ```fs``` 라이브러리 문서](https://gleam.run/stdlib/fs.html)

# 관련 링크 (See Also)

- [Gleam 공식 홈페이지](https://gleam.run)
- [Gleam 튜토리얼 (한국어 번역)](https://github.com/myungjaeyu/gleam-tutorial-kr)