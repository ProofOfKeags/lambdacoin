# Haskelling Bitcoin for Great Good

## Motivation

Throughout history, the way software has been written is a reflection of the environment it had to operate in. The tools
and processes we choose to develop and use were designed to make certain tradeoffs, and while certain tools and methods
may be pareto optimal against others, the vast majority of them involve making tradeoffs by sacrificing benefits that
matter less to our problem domain, in exchange for getting others that we value more strongly.

Full disclosure? I've only been out of college for a couple of years now, and as such a lot of my exposure to software
development has been through the lens of the internet and the web. Web software was an incredible boon to the startup
crowd. It meant that we could stop doing expensive waterfall releases in favor of punting things out the door as soon
as they began to look like they even slightly worked. The reason that we could do this is because if something broke
or we needed to ship an improvement, we just push new code, bounce the servers and we're up an running on a new version.
Bugs were cheap.

Then came Bitcoin.

Undoubtedly many of you have heard of Bitcoin or the other insanity that happened last year with the ICO madness. But
what is it? If we cut through all the buzzwords of "blockchain", "decentralized", and "peer-to-peer networks". Bitcoin
managed to create a piece of distributed software whereby every node in the system can agree about the current state
without being able to trust the other participants. This has all sorts of benefits from software resilience, to more
political benefits like minimizing corruption risk. The downside is that it presents us with a new era of software
design where updates are extremely difficult if not impossible to execute. To make matters worse, we have endowed these
software systems with several hundred billion dollars worth of value.

In other words, this kind of software more closely resembles flight software for aerospace than it does the web software
that a lot of us have been writing for the last 2 decades.

So why is that important, especially to a functional programming conference? Well the tools we choose to build software
where Failure is not an Option™ look a lot different than the ones we choose when mistakes can be quickly remedied.

Correctness is more important than speed of development in this industry. So what do we do here? Use Haskell, and other
things like it. Pure functional programming with strong static typing and denotational semantics gives us a powerful
set of tools to be able to get software correct, at the cost of having new developers being a little less productive
than they would be if they had used a scripting language like javascript. In the web days this may have been the wrong
tradeoff, in the trustware industry, I contend that those days are over.

## Proof of Concept

When I was learning Haskell, it didn't take long for me to become fascinated with the level of rigor that it enforces
on your understanding of your problem domain. But for the longest time I had trouble imagining how to solve more
substantial problems using it because it seemed to take away all of the tools I was used to using to get any important
work done. Now naturally, it is a complete myth that you can't build substantial software in Haskell and my goal for
this talk is to prove that we can build a toy cryptocurrency in a small enough codebase to be accessible to intermediate
Haskellers. Mostly because I AM an intermediate Haskeller.

## Ledger Structure

## Consensus Rules

## Peer to Peer Networking

## Proof of Work