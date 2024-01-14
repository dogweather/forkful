---
title:    "Fish Shell: रैंडम नंबर्स उत्पन्न करना"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Kyun

Aksar hume programing mein kuch random numbers ki zarurat padti hain, jaise ki kisi game ka score ya fir koi encryption key banane ke liye. Yeh random numbers aise banaye jaate hain jo unpredictable aur uniform ho, yani ki har number equal chances ke saath generate hone ke.

Is blog post mein hum aapko batayenge ki kaise aap Fish Shell mein random numbers generate kar sakte hain aur iske kya fayde hain.

# Kaise Karein

Fish Shell mein random numbers banane ke liye hame `math/rand` command ka istemal karna hoga. Is command se hum kisi bhi range mein random number generate kar sakte hain. Neeche diye gaye code examples mein hum 1 se 10 tak ke numbers generate karne ke tarike dekhenge.

```
Fish Shell mein random numbers generate karna:
```shell
math/rand 0 10
```

```
1 se 10 tak ke numbers generate karna:
```shell
math/rand 1 10
```

Aap chahe toh apne desired range mein numbers specify kar sakte hain. Iske alawa, aap `echo` command ka istemal karke bhi random numbers print kar sakte hain.

```
Random number print karna:
```shell
echo (math/rand)
```


# Deep Dive

Random numbers generate karna kitna easy hai, na? Lekin kya aapne kabhi socha hai ki yeh numbers kaise generate hote hain? Fish Shell mein `math/rand` command seed-based hai, yani ki yeh ek initial number se shuru hota hai aur uss number se numbers generate karne ka process follow karta hai.

Iske alawa, ismein `random-internal` library ka bhi istemal kiya jaata hai, jo C language mein based hai aur globally seeded hota hai. Isse random numbers generation ka process aur bhi secure aur efficient ban jata hai.

Aap chahe toh `help` command se bhi iske usage aur options check kar sakte hain.

# Dekhen Bhi

Agar aap aur bhi interesting Fish Shell tutorials ya commands jaanna chahte hain, toh neeche diye gaye links ko dekhein:

- Official Fish Shell website: https://fishshell.com/
- Fish Shell Github repository: https://github.com/fish-shell/fish-shell
- Fish Shell command cheat sheet: https://devhints.io/fish-shell
- Fish Shell forum: https://github.com/fish-shell/fish-shell/issues
- Fish Shell subreddit: https://www.reddit.com/r/fishshell/