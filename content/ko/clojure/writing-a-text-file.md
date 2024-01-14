---
title:    "Clojure: 텍스트 파일을 작성하기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것의 이유는 매우 간단합니다. 텍스트 파일을 사용하면 코드와 데이터를 저장하고 관리하는 것이 매우 쉽습니다. 또한 다른 사람과 코드를 공유하거나 저장하거나 백업하기에도 유용합니다.

## 방법

Clojure에서 텍스트 파일을 작성하는 것은 매우 쉽습니다. 먼저 ```(spit file-name content)``` 함수를 사용하여 파일 이름과 내용을 입력하여 파일을 만듭니다. 예를 들어, 다음과 같이 작성할 수 있습니다.

```Clojure
(spit "sample.txt" "Hello, world!")
```

이 코드를 실행하면 현재 작업 디렉토리에 "sample.txt"라는 파일이 생성되고 내용으로 "Hello, world!"가 들어갑니다. 또한 이미 존재하는 텍스트 파일에 내용을 추가하려는 경우에는 ```(spit file-name content :append true)```을 사용하면 됩니다.

## 심층 분석

텍스트 파일을 작성하는 것은 간단한 작업처럼 보일 수 있지만, 파일의 인코딩, 언어 설정 등 다른 설정 값을 고려해야 할 때가 있습니다. 이렇게 하려면 ```(spit file-name content :encoding "utf-8" :charset "utf-8")```와 같이 옵션을 추가하여 파일을 작성하면 됩니다.

## 또 다른 참고 문헌

- [Clojure 입문서](https://clojure.org/guides/getting_started)
- [Clojure 개발 환경 설정 가이드](https://gist.github.com/jcsims/351205)
- [Clojure 파일 관련 함수 문서](https://clojuredocs.org/clojure.java.io/spit)