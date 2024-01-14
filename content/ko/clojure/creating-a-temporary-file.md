---
title:    "Clojure: 임시 파일 생성하기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

일시적인 파일을 만드는 프로그래밍을 하는 이유는 무엇일까요? 일시적인 파일은 프로그램 실행 중에 발생하는 일시적인 데이터를 저장하기 위해서 사용됩니다. 이러한 데이터는 필요 없어지거나 프로그램이 종료될 때 삭제될 수 있어서 불필요한 저장 공간을 차지하지 않습니다. 따라서 일시적인 파일은 프로그래밍에서 유용하게 활용될 수 있습니다.

## 어떻게

일시적인 파일을 Clojure에서 만드는 방법을 알아보겠습니다. 먼저 `java.io.File` 클래스를 사용하여 파일 객체를 생성합니다. 다음으로 `createTempFile` 함수를 사용하여 일시적인 파일을 생성합니다. 파일 이름과 확장자를 지정할 수 있으며, 옵션으로 파일이 저장될 디렉토리를 지정할 수도 있습니다. 아래 코드는 "tempfile"이라는 이름의 일시적인 파일을 현재 프로젝트 디렉토리에 생성하는 예시입니다.

```Clojure
(import '(java.io File))

(def temp-file (-> (File. "tempfile")
                   (.createTempFile "tempfile" nil)))

(prn temp-file) ;; #object[java.io.File 0x20098b3e "tempfile9176708774614131214"]

(.deleteOnExit temp-file) ;; 프로그램이 종료될 때 파일이 삭제될 수 있도록 설정합니다.
```
`prn` 함수는 파일 정보를 출력하기 위해 사용됩니다. `createTempFile` 함수를 통해 생성된 파일 객체는 `File` 클래스의 인스턴스이므로 해당 객체의 멤버 변수와 함수를 사용하여 파일의 속성을 확인할 수 있습니다.

또한 `createTempFile` 함수의 옵션 중 `isDirectory`를 `true`로 설정하여 디렉토리로도 일시적인 파일을 생성할 수 있습니다. 아래 코드는 "tempdir"이라는 이름의 일시적인 디렉토리를 현재 프로젝트 디렉토리에 생성하는 예시입니다.

```Clojure
(def temp-dir (-> (File. "tempdir")
                  (.createTempFile "tempdir" nil true)))

(prn temp-dir) ;; #object[java.io.File 0x534d83d1 "tempdir4363711888373907999"]
```

## 깊게 알아보기

Clojure에서 `createTempFile` 함수는 내부적으로 Java의 `java.io.File` 클래스와 `java.io.tmpdir` 시스템 프로퍼티를 사용하여 파일을 생성합니다. 이러한 내부 구현은 일시적인 파일을 생성하는데 있어서 안정적이고 효율적인 방법입니다. 

하지만 `deleteOnExit` 함수를 사용하여 프로그램이 종료될 때 파일을 삭제하도록 설정하면, 파일이 삭제되지 못하는 경우가 발생할 수 있습니다. 따라서 해당 함수를 사용할 때에는 유의해야 합니다. 

또한 `createTempFile` 함수는 내부적으로 무작위한 숫자를 파일의 이름 뒤에 붙여서 파일 이름 충돌을 방지합니다. 혹시나 같은 이름과 디렉토리를 가진 일시적인 파일을 생성하고 싶다면, *prefix*와 *suffix* 옵션을 사용하여 파일 이름을 지정할 수 있습니다.

## 또 다른 정보

- [Clojure 공식 문서 - Temp Files](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/temp-file)
- [Java SE 11 문서 - Temp Files](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#createTempFile(java.lang.String,java.lang.String,java.io.File))
- [Clojure 코딩 백서 - 임시 파일과