---
title:    "Go: एक टेक्स्ट फाइल लिखना"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

Kya aapne kabhi socha hai ki kisi text file ko likhna kyun zaruri hai? Agar aap ek Go programmer hai, toh text files aapke liye bahut important hai. Text files ke through hum apne data ko store kar sakte hai aur badhne wale samay mein usse access bhi kar sakte hai. Is blog post mein hum dekhenge ki kaise aap Go programming language mein text files ko likh sakte hai.

## कैसे

```Go
func main() {
    // File create karne ke liye
    file, err := os.Create("sample.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer file.Close()

    // Text file mein data likhne ke liye
    data := "Ye ek sample text file hai."
    fmt.Fprintln(file, data)

    // Text file ko read karne ke liye
    b, err := ioutil.ReadFile("sample.txt")
    if err != nil {
        fmt.Println(err)
        return
    }

    // Output print kare
    fmt.Println(string(b))
}
```

```
Output:
Ye ek sample text file hai.
```

Is code example se aap dekh sakte hai ki hum kaise ek text file create kar sakte hai, usme data likh sakte hai aur phir use padh sakte hai. `io/ioutil` library hume file ko padhne aur likhne mein madad karta hai.

## गहराई में झाँक

Text file likhna asan hai, lekin isme kuch gahrai hai jise jaanne ke zarurat hai. Text file ke andar data ko store karne ke alawa, hume file ko open aur close karna bhi zaruri hota hai. Sahi jagah par file ko close na karne se humare program mein memory leaks ho sakte hai. Isliye, `defer` keyword ka istemal karke hume file ko automatically close karna chahiye. Iske alawa, hume error handling bhi dhyan rakhna chahiye taki program beech mein crash na ho. 

Text file mein data likhna aur padhna asan hai, lekin file ka table of contents (TOC) banaana aur use update karna thoda difficult ho sakta hai. Iske liye hume `bufio` library ka istemal karna chahiye.

## देखें भी

- [Go documentation on file handling](https://golang.org/pkg/os)
- [More examples of file handling in Go](https://www.golangprograms.com/go-language/file-handling.html)
- [Using bufio library for reading and writing files](https://tutorialedge.net/golang/reading-console-input-golang)