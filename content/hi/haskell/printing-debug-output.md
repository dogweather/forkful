---
title:    "Haskell: डीबग आउटपुट छापना"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## क्यों

Debug आउटपुट प्रिंट करने का सबसे साधारण कारण यह है कि इससे आपको अपने कोड में दिख रही समस्याओं को आसानी से पहचानने में मदद मिलती है।

## कैसे करें

आप Haskell में `Debug.Trace` लाइब्रेरी का उपयोग करके, `trace` फ़ंक्शन को अपने कोड में डालकर debug आउटपुट प्रिंट कर सकते हैं। नीचे एक उदाहरण दिया गया है:

```Haskell
import Debug.Trace

square :: Int -> Int
square x = trace ("Calculating square of " ++ show x) (x * x)

main :: IO ()
main = do
  let num = 5
  print $ "The square of " ++ show num ++ " is " ++ show (square num)
```

आउटपुट:

```
"Calculating square of 5"
"The square of 5 is 25"
```

## गहराई में खोजें

Debug आउटपुट प्रिंट करने के अलावा, आप भी `traceShow` फ़ंक्शन का उपयोग कर सकते हैं जो आपको अपने डेटा की गहराई में जानकारी देता है। नीचे एक उदाहरण दिया गया है:

```Haskell
import Debug.Trace

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

main :: IO ()
main = do
  let person = Person "John" 25
  print $ traceShow person "Person:"
```

आउटपुट:

```
Person: (Person {name = "John", age = 25})
```

## और भी देखें

- [Haskell Wiki: Debugging techniques](https://wiki.haskell.org/Debugging_techniques)
- [Haskell Debugging in GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugger.html)
- [Debugging in Haskell with the `Debug.Trace` Module](https://daveaglick.com/posts/debugging-in-haskell-with-the-debug-trace-module)