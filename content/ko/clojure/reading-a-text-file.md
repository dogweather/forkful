---
title:    "Clojure: 텍스트 파일 읽기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 읽는 프로그래밍은 데이터를 처리하는 필수적인 작업입니다. 클로저 프로그래밍을 할 때 텍스트 파일을 읽는 방법을 배우면 다양한 작업에 유용하게 활용할 수 있습니다.

## 하는 법
먼저 클로저에서 텍스트 파일을 읽기 위해 다음과 같은 코드를 작성해야 합니다.
```
 (with-open [reader (clojure.java.io/reader "example-file.txt")] 
    (doseq [line (line-seq reader)] 
        (println line)))
```

위의 코드는 `example-file.txt` 파일을 열어서 한 줄씩 읽은 후, 해당 줄을 출력하는 작업을 합니다. 이를 실행하면 다음과 같은 결과가 나옵니다.
```
This is line 1
This is line 2
This is line 3
...
```

## 깊게 들어가기
텍스트 파일을 읽을 때 주의해야 할 점은 파일 크기에 따라 메모리 사용량이 증가할 수 있다는 것입니다. 따라서 크기가 큰 파일을 읽을 때에는 `line-seq` 대신 `line-seq-lazy` 함수를 사용하는 것이 좋습니다. 이 함수는 파일의 내용을 한 줄씩 읽는 대신 필요할 때만 읽어오기 때문에 메모리 사용량을 낮출 수 있습니다.

또한, 텍스트 파일에서 읽어온 데이터를 다룰 때 문제가 되는 것이 많은 양의 데이터를 매번 스트링으로 반환하는 것입니다. 이를 해결하기 위해 `clojure.string/split` 함수를 사용하여 데이터를 원하는 형태로 쉽게 변환할 수 있습니다.

## 본문 끝
마지막으로, 여러분은 이제 클로저를 사용하여 텍스트 파일을 읽는 방법을 배웠습니다. 이제 다양한 데이터를 처리할 때 유용하게 활용할 수 있을 것입니다. 추가적으로 관련된 내용을 자세히 공부하고 싶다면 아래의 링크를 참고해보세요.

## 관련자료
- https://clojure.or.kr/
- https://www.clojure.org/
- https://github.com/clojure/clojure