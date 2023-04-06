# prop-unit

Conveniences for using Hedgehog as a unit test runner

It's a pain to share assertions between Hedgehog and HUnit. My solution is to basically turn unit tests into really simple property-based tests!

Useful environment variables:

    # Turn on "debug mode" (default 0)
    # Changes buffering and threading for easier debugging
    PROP_UNIT_DEBUG=1

    # Increase test examples (default 100)
    # Only has effect on true property-based tests
    PROP_UNIT_LIMIT=1000
