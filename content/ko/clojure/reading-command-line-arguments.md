---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
명령행 인자를 읽는 것은 프로그램을 시작할 때 사용자로부터 전달받는 인자를 읽는 것입니다. 이를 통해 프로그램의 실행 방식을 사용자 정의할 수 있습니다.

##어떻게 할 것인가:
```Clojure 
(defn -main [& args]
  (println "입력한 인자: " args))
```
이 매우 간단한 Clojure 프로그램을 사용하여 명령행 인자를 읽을 수 있습니다.

프로그램을 다음과 같이 실행하면:

```bash
$ lein run 인자1 인자2 인자3
```
다음과 같은 출력을 볼 수 있습니다:

```
입력한 인자: (인자1 인자2 인자3)
```

## 행 수
명령 행 인자는 Unix 시스템에서 프로그래밍 및 자동화 작업을 실행할 때 일반적으로 사용되었습니다. 이는 스크립트에 다양성을 추가하고 작업을 쉽게 자동화 할 수 있게 해줍니다. 

이를 대체할 수 있는 방법은 프로그램 내부의 동적 입력 (예: 사용자 입력 또는 파일 입력)이지만, 명령행 인자는 프로그램이 시작할 때 한번만 입력 받기 때문에 더 향상된 효율성과 성능을 가지고 있습니다.

명령행 인자는 JVM에 의해 String 배열로 전달되며, Clojure는 이 배열을 가변 인자 목록으로 매핑하여 `-main` 함수에 제공합니다.

## 참조
Crojure 명령행 인자에 대한 더 깊은 이해를 위해 다음 링크를 확인하십시오:
- [Clojure 공식 문서](https://clojure.org/)
- [Command Line Arguments in Clojure](https://clojuredocs.org/clojure.core/*command-line-args*)
- [Command-line argument parsing in Clojure](http://web.archive.org/web/20100621051006/http://programming-puzzler.blogspot.com/2009/05/command-line-argument-parsing-in.html)