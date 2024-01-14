---
title:                "Clojure: 텍스트 파일 작성하기"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜 

텍스트 파일을 쓰는 이유는 프로그래밍에서 중요한 부분입니다. 이 글에서는 Clojure를 사용하여 텍스트 파일을 쓰는 방법과 깊이있는 정보를 알려드리겠습니다.

# 어떻게

```Clojure
(with-open [file (clojure.java.io/writer "파일이름.txt")]
  (.write file "이것은 텍스트 파일에 쓸 내용입니다."))
```

위의 코드는 파일 이름을 지정하고, 파일을 열고, 지정한 내용을 파일에 쓰는 코드입니다. 파일을 열고 나서는 `.write` 함수를 사용하여 원하는 내용을 쓸 수 있습니다. 이제 이 코드를 실행하고 `파일이름.txt` 파일을 열어보면 지정한 내용이 들어있는 것을 볼 수 있습니다.

# 깊이 알아보기

파일을 쓰는 방법은 한 가지가 아닙니다. Clojure에는 다른 방식으로 파일을 쓸 수 있도록 다양한 함수가 있습니다. 예를 들어, `spit` 함수를 사용하면 파일을 열지 않고도 바로 내용을 쓸 수 있습니다. 또한 파일에 내용을 추가할 수도 있습니다. 이 외에도 다양한 방법이 있으니 자세한 내용은 Clojure 공식 문서를 참고해주세요.

# 또 다른 정보

## 관련 참고 자료

- [Clojure 공식 문서](https://clojure.org/)
- [10분 Clojure 튜토리얼](https://yagom.github.io/resources/clojure/10-min-clojure/)
- [ClojureScript로 간단한 웹사이트 만들기](https://www.jacopobeschi.com/post/clojurescript-tutorial-1-introduction/)
- [Clojure로 게임 만들기](https://gist.github.com/mudphone/0073748a9f2eac1df46d5ff9d94b06a5)

# 참고

이 글은 한국어로 Clojure를 처음 공부하는 분들을 위해 작성되었습니다. 여러분의 학습에 도움이 되길 바랍니다.